let prompt = Ex_OddlyRecursive_prompt.prompt

let exercise : Exercise.spec =
  {
    next_id = 683;
    title = "Oddly Recursive";
    version = 1;
    module_name = "Ex_OddlyRecursive";
    prompt;
    point_distribution =
      { test_validation = 1; mutation_testing = 1; impl_grading = 2 };
    prelude =
      {
        selection = { focus = Left; content = [] };
        backpack = [];
        relatives =
          {
            siblings =
              ( [
                  Tile
                    {
                      id = 3;
                      label = [ "let"; "="; "in" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat; Exp ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 5; sort = Exp } );
                        };
                      shards = [ 0; 1; 2 ];
                      children =
                        [
                          [
                            Secondary { id = 4; content = Whitespace " " };
                            Tile
                              {
                                id = 8;
                                label = [ "not" ];
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
                            Secondary { id = 9; content = Whitespace " " };
                            Tile
                              {
                                id = 10;
                                label = [ ":" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 10; sort = Pat },
                                        { shape = Concave 10; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 12; content = Whitespace " " };
                            Tile
                              {
                                id = 16;
                                label = [ "Bool" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 17; content = Whitespace " " };
                            Tile
                              {
                                id = 20;
                                label = [ "->" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 16; sort = Typ },
                                        { shape = Concave 16; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 21; content = Whitespace " " };
                            Tile
                              {
                                id = 25;
                                label = [ "Bool" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 26; content = Whitespace " " };
                          ];
                          [
                            Secondary
                              { id = 28; content = Whitespace "\226\143\142" };
                            Tile
                              {
                                id = 32;
                                label = [ "fun"; "->" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [ Pat ];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Concave 7; sort = Exp } );
                                  };
                                shards = [ 0; 1 ];
                                children =
                                  [
                                    [
                                      Secondary
                                        { id = 33; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 35;
                                          label = [ "x" ];
                                          mold =
                                            {
                                              out = Pat;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Pat },
                                                  { shape = Convex; sort = Pat }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 36; content = Whitespace " " };
                                    ];
                                  ];
                              };
                            Secondary
                              { id = 39; content = Whitespace "\226\143\142" };
                            Tile
                              {
                                id = 42;
                                label = [ "if"; "then"; "else" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [ Exp; Exp ];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Concave 9; sort = Exp } );
                                  };
                                shards = [ 0; 1; 2 ];
                                children =
                                  [
                                    [
                                      Secondary
                                        { id = 43; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 45;
                                          label = [ "x" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 48; content = Whitespace " " };
                                    ];
                                    [
                                      Secondary
                                        { id = 52; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 57;
                                          label = [ "false" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 60; content = Whitespace " " };
                                    ];
                                  ];
                              };
                            Secondary { id = 64; content = Whitespace " " };
                            Tile
                              {
                                id = 68;
                                label = [ "true" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Convex; sort = Exp } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 72; content = Whitespace " " };
                            Secondary { id = 69; content = Whitespace " " };
                            Secondary
                              { id = 70; content = Whitespace "\226\143\142" };
                          ];
                        ];
                    };
                  Secondary { id = 74; content = Whitespace " " };
                  Secondary { id = 75; content = Whitespace " " };
                ],
                [ Grout { id = 73; shape = Convex } ] );
            ancestors = [];
          };
        caret = Outer;
      };
    correct_impl =
      {
        selection = { focus = Left; content = [] };
        backpack = [];
        relatives =
          {
            siblings =
              ( [
                  Tile
                    {
                      id = 79;
                      label = [ "let"; "="; "in" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat; Exp ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 5; sort = Exp } );
                        };
                      shards = [ 0; 1; 2 ];
                      children =
                        [
                          [
                            Secondary { id = 80; content = Whitespace " " };
                            Tile
                              {
                                id = 84;
                                label = [ "odd" ];
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
                            Tile
                              {
                                id = 85;
                                label = [ ":" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 10; sort = Pat },
                                        { shape = Concave 10; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Tile
                              {
                                id = 89;
                                label = [ "Int" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Tile
                              {
                                id = 92;
                                label = [ "->" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 16; sort = Typ },
                                        { shape = Concave 16; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Tile
                              {
                                id = 96;
                                label = [ "Bool" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 97; content = Whitespace " " };
                          ];
                          [
                            Secondary
                              { id = 99; content = Whitespace "\226\143\142" };
                            Tile
                              {
                                id = 103;
                                label = [ "fun"; "->" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [ Pat ];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Concave 7; sort = Exp } );
                                  };
                                shards = [ 0; 1 ];
                                children =
                                  [
                                    [
                                      Secondary
                                        { id = 104; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 106;
                                          label = [ "x" ];
                                          mold =
                                            {
                                              out = Pat;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Pat },
                                                  { shape = Convex; sort = Pat }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 107; content = Whitespace " " };
                                    ];
                                  ];
                              };
                            Secondary
                              { id = 110; content = Whitespace "\226\143\142" };
                            Tile
                              {
                                id = 113;
                                label = [ "if"; "then"; "else" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [ Exp; Exp ];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Concave 9; sort = Exp } );
                                  };
                                shards = [ 0; 1; 2 ];
                                children =
                                  [
                                    [
                                      Secondary
                                        { id = 114; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 116;
                                          label = [ "x" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 117; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 118;
                                          label = [ "<" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 13;
                                                    sort = Exp;
                                                  },
                                                  {
                                                    shape = Concave 13;
                                                    sort = Exp;
                                                  } );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 120; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 121;
                                          label = [ "0" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 125; content = Whitespace " " };
                                      Secondary
                                        { id = 122; content = Whitespace " " };
                                      Secondary
                                        {
                                          id = 123;
                                          content = Whitespace "\226\143\142";
                                        };
                                    ];
                                    [
                                      Secondary
                                        { id = 129; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 132;
                                          label = [ "odd" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Tile
                                        {
                                          id = 133;
                                          label = [ "("; ")" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [ Exp ];
                                              nibs =
                                                ( {
                                                    shape = Concave 21;
                                                    sort = Exp;
                                                  },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0; 1 ];
                                          children =
                                            [
                                              [
                                                Tile
                                                  {
                                                    id = 135;
                                                    label = [ "-" ];
                                                    mold =
                                                      {
                                                        out = Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Exp;
                                                            },
                                                            {
                                                              shape = Concave 20;
                                                              sort = Exp;
                                                            } );
                                                      };
                                                    shards = [ 0 ];
                                                    children = [];
                                                  };
                                                Tile
                                                  {
                                                    id = 136;
                                                    label = [ "x" ];
                                                    mold =
                                                      {
                                                        out = Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Exp;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Exp;
                                                            } );
                                                      };
                                                    shards = [ 0 ];
                                                    children = [];
                                                  };
                                              ];
                                            ];
                                        };
                                      Secondary
                                        { id = 140; content = Whitespace " " };
                                      Secondary
                                        { id = 137; content = Whitespace " " };
                                      Secondary
                                        {
                                          id = 138;
                                          content = Whitespace "\226\143\142";
                                        };
                                    ];
                                  ];
                              };
                            Secondary { id = 144; content = Whitespace " " };
                            Tile
                              {
                                id = 147;
                                label = [ "if"; "then"; "else" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [ Exp; Exp ];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Concave 9; sort = Exp } );
                                  };
                                shards = [ 0; 1; 2 ];
                                children =
                                  [
                                    [
                                      Secondary
                                        { id = 148; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 150;
                                          label = [ "x" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 151; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 154;
                                          label = [ "==" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 13;
                                                    sort = Exp;
                                                  },
                                                  {
                                                    shape = Concave 13;
                                                    sort = Exp;
                                                  } );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 155; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 156;
                                          label = [ "0" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 159; content = Whitespace " " };
                                    ];
                                    [
                                      Secondary
                                        { id = 163; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 168;
                                          label = [ "false" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 172; content = Whitespace " " };
                                      Secondary
                                        { id = 169; content = Whitespace " " };
                                      Secondary
                                        {
                                          id = 170;
                                          content = Whitespace "\226\143\142";
                                        };
                                    ];
                                  ];
                              };
                            Secondary { id = 176; content = Whitespace " " };
                            Tile
                              {
                                id = 179;
                                label = [ "not" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Convex; sort = Exp } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Tile
                              {
                                id = 180;
                                label = [ "("; ")" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [ Exp ];
                                    nibs =
                                      ( { shape = Concave 21; sort = Exp },
                                        { shape = Convex; sort = Exp } );
                                  };
                                shards = [ 0; 1 ];
                                children =
                                  [
                                    [
                                      Tile
                                        {
                                          id = 184;
                                          label = [ "odd" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Tile
                                        {
                                          id = 185;
                                          label = [ "("; ")" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [ Exp ];
                                              nibs =
                                                ( {
                                                    shape = Concave 21;
                                                    sort = Exp;
                                                  },
                                                  { shape = Convex; sort = Exp }
                                                );
                                            };
                                          shards = [ 0; 1 ];
                                          children =
                                            [
                                              [
                                                Tile
                                                  {
                                                    id = 187;
                                                    label = [ "x" ];
                                                    mold =
                                                      {
                                                        out = Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Exp;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Exp;
                                                            } );
                                                      };
                                                    shards = [ 0 ];
                                                    children = [];
                                                  };
                                                Grout
                                                  { id = 191; shape = Concave };
                                                Tile
                                                  {
                                                    id = 190;
                                                    label = [ "-1" ];
                                                    mold =
                                                      {
                                                        out = Exp;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Exp;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Exp;
                                                            } );
                                                      };
                                                    shards = [ 0 ];
                                                    children = [];
                                                  };
                                              ];
                                            ];
                                        };
                                    ];
                                  ];
                              };
                            Secondary { id = 195; content = Whitespace " " };
                            Secondary { id = 192; content = Whitespace " " };
                            Secondary
                              { id = 193; content = Whitespace "\226\143\142" };
                          ];
                        ];
                    };
                  Secondary { id = 197; content = Whitespace " " };
                  Secondary { id = 198; content = Whitespace " " };
                ],
                [ Grout { id = 196; shape = Convex } ] );
            ancestors = [];
          };
        caret = Outer;
      };
    your_tests =
      {
        tests =
          {
            selection = { focus = Left; content = [] };
            backpack = [];
            relatives =
              {
                siblings =
                  ( [
                      Tile
                        {
                          id = 203;
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary { id = 204; content = Whitespace " " };
                                Tile
                                  {
                                    id = 208;
                                    label = [ "not" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id = 209;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 21; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 215;
                                              label = [ "false" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary { id = 218; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 220;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 8; sort = Exp },
                                  { shape = Concave 8; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 222; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 227;
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary { id = 228; content = Whitespace " " };
                                Tile
                                  {
                                    id = 232;
                                    label = [ "not" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id = 233;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 21; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 237;
                                              label = [ "not" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id = 238;
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 21;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0; 1 ];
                                              children =
                                                [
                                                  [
                                                    Tile
                                                      {
                                                        id = 243;
                                                        label = [ "true" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                  ];
                                                ];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary { id = 246; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 248;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 8; sort = Exp },
                                  { shape = Concave 8; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary { id = 250; content = Whitespace " " };
                      Secondary
                        { id = 251; content = Whitespace "\226\143\142" };
                      Secondary { id = 252; content = Whitespace " " };
                    ],
                    [ Grout { id = 249; shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
          };
        required = 6;
        provided = 2;
      };
    your_impl =
      {
        selection = { focus = Left; content = [] };
        backpack = [];
        relatives =
          {
            siblings =
              ( [
                  Tile
                    {
                      id = 256;
                      label = [ "let"; "="; "in" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat; Exp ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 5; sort = Exp } );
                        };
                      shards = [ 0; 1; 2 ];
                      children =
                        [
                          [
                            Secondary { id = 257; content = Whitespace " " };
                            Tile
                              {
                                id = 261;
                                label = [ "odd" ];
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
                            Tile
                              {
                                id = 262;
                                label = [ ":" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 10; sort = Pat },
                                        { shape = Concave 10; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 264; content = Whitespace " " };
                            Tile
                              {
                                id = 267;
                                label = [ "Int" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 268; content = Whitespace " " };
                            Tile
                              {
                                id = 271;
                                label = [ "->" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 16; sort = Typ },
                                        { shape = Concave 16; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 272; content = Whitespace " " };
                            Tile
                              {
                                id = 276;
                                label = [ "Bool" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 277; content = Whitespace " " };
                          ];
                          [
                            Secondary
                              { id = 279; content = Whitespace "\226\143\142" };
                            Tile
                              {
                                id = 283;
                                label = [ "fun"; "->" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [ Pat ];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Concave 7; sort = Exp } );
                                  };
                                shards = [ 0; 1 ];
                                children =
                                  [
                                    [
                                      Secondary
                                        { id = 284; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 286;
                                          label = [ "n" ];
                                          mold =
                                            {
                                              out = Pat;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Pat },
                                                  { shape = Convex; sort = Pat }
                                                );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Secondary
                                        { id = 287; content = Whitespace " " };
                                    ];
                                  ];
                              };
                            Grout { id = 295; shape = Convex };
                            Secondary { id = 291; content = Whitespace " " };
                            Secondary { id = 292; content = Whitespace " " };
                            Secondary
                              { id = 293; content = Whitespace "\226\143\142" };
                          ];
                        ];
                    };
                  Secondary { id = 297; content = Whitespace " " };
                  Secondary { id = 298; content = Whitespace " " };
                ],
                [ Grout { id = 296; shape = Convex } ] );
            ancestors = [];
          };
        caret = Outer;
      };
    hidden_bugs =
      [
        {
          impl =
            {
              selection = { focus = Left; content = [] };
              backpack = [];
              relatives =
                {
                  siblings =
                    ( [
                        Tile
                          {
                            id = 302;
                            label = [ "let"; "="; "in" ];
                            mold =
                              {
                                out = Exp;
                                in_ = [ Pat; Exp ];
                                nibs =
                                  ( { shape = Convex; sort = Exp },
                                    { shape = Concave 5; sort = Exp } );
                              };
                            shards = [ 0; 1; 2 ];
                            children =
                              [
                                [
                                  Secondary
                                    { id = 303; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 307;
                                      label = [ "odd" ];
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
                                  Tile
                                    {
                                      id = 308;
                                      label = [ ":" ];
                                      mold =
                                        {
                                          out = Pat;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 10; sort = Pat },
                                              { shape = Concave 10; sort = Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 310; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 313;
                                      label = [ "Int" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Typ },
                                              { shape = Convex; sort = Typ } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 314; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 317;
                                      label = [ "->" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 16; sort = Typ },
                                              { shape = Concave 16; sort = Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 318; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 322;
                                      label = [ "Bool" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Typ },
                                              { shape = Convex; sort = Typ } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 323; content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    {
                                      id = 325;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 329;
                                      label = [ "fun"; "->" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Pat ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 7; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 330;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 332;
                                                label = [ "x" ];
                                                mold =
                                                  {
                                                    out = Pat;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Pat;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Pat;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 333;
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 336; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 341;
                                      label = [ "false" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Convex; sort = Exp } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 345; content = Whitespace " " };
                                  Secondary
                                    { id = 342; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 343;
                                      content = Whitespace "\226\143\142";
                                    };
                                ];
                              ];
                          };
                        Secondary { id = 347; content = Whitespace " " };
                        Secondary { id = 348; content = Whitespace " " };
                      ],
                      [ Grout { id = 346; shape = Convex } ] );
                  ancestors = [];
                };
              caret = Outer;
            };
          hint = "always returns false";
        };
        {
          impl =
            {
              selection = { focus = Left; content = [] };
              backpack = [];
              relatives =
                {
                  siblings =
                    ( [
                        Tile
                          {
                            id = 352;
                            label = [ "let"; "="; "in" ];
                            mold =
                              {
                                out = Exp;
                                in_ = [ Pat; Exp ];
                                nibs =
                                  ( { shape = Convex; sort = Exp },
                                    { shape = Concave 5; sort = Exp } );
                              };
                            shards = [ 0; 1; 2 ];
                            children =
                              [
                                [
                                  Secondary
                                    { id = 353; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 357;
                                      label = [ "odd" ];
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
                                  Tile
                                    {
                                      id = 358;
                                      label = [ ":" ];
                                      mold =
                                        {
                                          out = Pat;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 10; sort = Pat },
                                              { shape = Concave 10; sort = Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 360; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 363;
                                      label = [ "Int" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Typ },
                                              { shape = Convex; sort = Typ } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 364; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 367;
                                      label = [ "->" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 16; sort = Typ },
                                              { shape = Concave 16; sort = Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 368; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 372;
                                      label = [ "Bool" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Typ },
                                              { shape = Convex; sort = Typ } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 373; content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    {
                                      id = 375;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 379;
                                      label = [ "fun"; "->" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Pat ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 7; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 380;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 382;
                                                label = [ "x" ];
                                                mold =
                                                  {
                                                    out = Pat;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Pat;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Pat;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 383;
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 386; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 390;
                                      label = [ "true" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Convex; sort = Exp } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 394; content = Whitespace " " };
                                  Secondary
                                    { id = 391; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 392;
                                      content = Whitespace "\226\143\142";
                                    };
                                ];
                              ];
                          };
                        Secondary { id = 396; content = Whitespace " " };
                        Secondary { id = 397; content = Whitespace " " };
                      ],
                      [ Grout { id = 395; shape = Convex } ] );
                  ancestors = [];
                };
              caret = Outer;
            };
          hint = "always returns true";
        };
        {
          impl =
            {
              selection = { focus = Left; content = [] };
              backpack = [];
              relatives =
                {
                  siblings =
                    ( [
                        Tile
                          {
                            id = 401;
                            label = [ "let"; "="; "in" ];
                            mold =
                              {
                                out = Exp;
                                in_ = [ Pat; Exp ];
                                nibs =
                                  ( { shape = Convex; sort = Exp },
                                    { shape = Concave 5; sort = Exp } );
                              };
                            shards = [ 0; 1; 2 ];
                            children =
                              [
                                [
                                  Secondary
                                    { id = 402; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 406;
                                      label = [ "odd" ];
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
                                  Tile
                                    {
                                      id = 407;
                                      label = [ ":" ];
                                      mold =
                                        {
                                          out = Pat;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 10; sort = Pat },
                                              { shape = Concave 10; sort = Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 409; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 412;
                                      label = [ "Int" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Typ },
                                              { shape = Convex; sort = Typ } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 413; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 416;
                                      label = [ "->" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 16; sort = Typ },
                                              { shape = Concave 16; sort = Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 417; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 421;
                                      label = [ "Bool" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Typ },
                                              { shape = Convex; sort = Typ } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 422; content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    {
                                      id = 424;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 428;
                                      label = [ "fun"; "->" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Pat ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 7; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 429;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 431;
                                                label = [ "x" ];
                                                mold =
                                                  {
                                                    out = Pat;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Pat;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Pat;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 432;
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 435; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 438;
                                      label = [ "if"; "then"; "else" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp; Exp ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 9; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1; 2 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 439;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 441;
                                                label = [ "x" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 442;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 443;
                                                label = [ "<" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 13;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 13;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 445;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 446;
                                                label = [ "0" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 449;
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = 453;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 456;
                                                label = [ "odd" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Tile
                                              {
                                                id = 457;
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [ Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 21;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0; 1 ];
                                                children =
                                                  [
                                                    [
                                                      Tile
                                                        {
                                                          id = 459;
                                                          label = [ "-" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Concave 20;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id = 460;
                                                          label = [ "x" ];
                                                          mold =
                                                            {
                                                              out = Exp;
                                                              in_ = [];
                                                              nibs =
                                                                ( {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
                                                                  },
                                                                  {
                                                                    shape =
                                                                      Convex;
                                                                    sort = Exp;
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
                                                id = 464;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 461;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 462;
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 468; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 471;
                                      label = [ "if"; "then"; "else" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp; Exp ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 9; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1; 2 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 472;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 474;
                                                label = [ "x" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 475;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 478;
                                                label = [ "==" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 13;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 13;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 479;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 480;
                                                label = [ "0" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 483;
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = 487;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 491;
                                                label = [ "true" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 495;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 492;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 493;
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 499; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 502;
                                      label = [ "if"; "then"; "else" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp; Exp ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 9; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1; 2 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 503;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 505;
                                                label = [ "x" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 506;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 509;
                                                label = [ "==" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 13;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 13;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 510;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 511;
                                                label = [ "1" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 514;
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = 518;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 522;
                                                label = [ "true" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 526;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 523;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 524;
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 530; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 533;
                                      label = [ "odd" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Convex; sort = Exp } );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Tile
                                    {
                                      id = 534;
                                      label = [ "("; ")" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp ];
                                          nibs =
                                            ( { shape = Concave 21; sort = Exp },
                                              { shape = Convex; sort = Exp } );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Tile
                                              {
                                                id = 536;
                                                label = [ "x" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 537;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 538;
                                                label = [ "-" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 17;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 17;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 540;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 541;
                                                label = [ "1" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Convex;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 544; content = Whitespace " " };
                                ];
                              ];
                          };
                        Secondary
                          { id = 546; content = Whitespace "\226\143\142" };
                        Secondary { id = 547; content = Whitespace " " };
                      ],
                      [ Grout { id = 545; shape = Convex } ] );
                  ancestors = [];
                };
              caret = Outer;
            };
          hint = "incorrect base case";
        };
      ];
    hidden_tests =
      {
        tests =
          {
            selection = { focus = Left; content = [] };
            backpack = [];
            relatives =
              {
                siblings =
                  ( [
                      Tile
                        {
                          id = 552;
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary { id = 553; content = Whitespace " " };
                                Tile
                                  {
                                    id = 557;
                                    label = [ "not" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id = 558;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 21; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 562;
                                              label = [ "odd" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id = 563;
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 21;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0; 1 ];
                                              children =
                                                [
                                                  [
                                                    Tile
                                                      {
                                                        id = 565;
                                                        label = [ "0" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                  ];
                                                ];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary { id = 568; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 570;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 8; sort = Exp },
                                  { shape = Concave 8; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 572; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 577;
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary { id = 578; content = Whitespace " " };
                                Tile
                                  {
                                    id = 582;
                                    label = [ "odd" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id = 583;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 21; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 585;
                                              label = [ "1" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary { id = 588; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 590;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 8; sort = Exp },
                                  { shape = Concave 8; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 592; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 597;
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary { id = 598; content = Whitespace " " };
                                Tile
                                  {
                                    id = 602;
                                    label = [ "not" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id = 603;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 21; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 607;
                                              label = [ "odd" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id = 608;
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 21;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0; 1 ];
                                              children =
                                                [
                                                  [
                                                    Tile
                                                      {
                                                        id = 610;
                                                        label = [ "2" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                  ];
                                                ];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary { id = 613; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 615;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 8; sort = Exp },
                                  { shape = Concave 8; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 617; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 622;
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary { id = 623; content = Whitespace " " };
                                Tile
                                  {
                                    id = 627;
                                    label = [ "odd" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id = 628;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 21; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 630;
                                              label = [ "3" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary { id = 633; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 635;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 8; sort = Exp },
                                  { shape = Concave 8; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 637; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 642;
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary { id = 643; content = Whitespace " " };
                                Tile
                                  {
                                    id = 647;
                                    label = [ "not" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id = 648;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 21; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 652;
                                              label = [ "odd" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                          Tile
                                            {
                                              id = 653;
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 21;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0; 1 ];
                                              children =
                                                [
                                                  [
                                                    Tile
                                                      {
                                                        id = 656;
                                                        label = [ "42" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape = Convex;
                                                                  sort = Exp;
                                                                } );
                                                          };
                                                        shards = [ 0 ];
                                                        children = [];
                                                      };
                                                  ];
                                                ];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary { id = 659; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 661;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 8; sort = Exp },
                                  { shape = Concave 8; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary { id = 663; content = Whitespace " " };
                      Secondary
                        { id = 664; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 669;
                          label = [ "test"; "end" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Convex; sort = Exp } );
                            };
                          shards = [ 0; 1 ];
                          children =
                            [
                              [
                                Secondary { id = 670; content = Whitespace " " };
                                Tile
                                  {
                                    id = 674;
                                    label = [ "odd" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Tile
                                  {
                                    id = 675;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 21; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 678;
                                              label = [ "27" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Convex;
                                                        sort = Exp;
                                                      } );
                                                };
                                              shards = [ 0 ];
                                              children = [];
                                            };
                                        ];
                                      ];
                                  };
                                Secondary { id = 681; content = Whitespace " " };
                              ];
                            ];
                        };
                    ],
                    [] );
                ancestors = [];
              };
            caret = Outer;
          };
        hints = [ "zero" ];
      };
    syntax_tests =
      [
        ("not is applied", Haz3lschool.SyntaxTest.var_applied "not");
        ("odd is recursive", Haz3lschool.SyntaxTest.is_recursive "odd");
      ];
  }
