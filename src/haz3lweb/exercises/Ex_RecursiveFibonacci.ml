let prompt = Ex_RecursiveFibonacci_prompt.prompt

let exercise : Exercise.spec =
  {
    next_id = 612;
    title = "Recursive Fibonacci";
    version = 1;
    module_name = "Ex_RecursiveFibonacci";
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
              ( [ Secondary { id = 0; content = Whitespace " " } ],
                [ Grout { id = 0; shape = Convex } ] );
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
                      id = 4;
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
                            Secondary { id = 5; content = Whitespace " " };
                            Tile
                              {
                                id = 9;
                                label = [ "fib" ];
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
                                id = 15;
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
                            Secondary { id = 16; content = Whitespace " " };
                            Tile
                              {
                                id = 19;
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
                            Secondary { id = 20; content = Whitespace " " };
                            Tile
                              {
                                id = 23;
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
                            Secondary { id = 24; content = Whitespace " " };
                          ];
                          [
                            Secondary { id = 26; content = Whitespace " " };
                            Secondary
                              { id = 27; content = Whitespace "\226\143\142" };
                            Tile
                              {
                                id = 31;
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
                                        { id = 32; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 34;
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
                                        { id = 35; content = Whitespace " " };
                                    ];
                                  ];
                              };
                            Secondary { id = 38; content = Whitespace " " };
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
                                        { id = 46; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 47;
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
                                        { id = 49; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 50;
                                          label = [ "2" ];
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
                                        { id = 53; content = Whitespace " " };
                                    ];
                                    [
                                      Secondary
                                        { id = 57; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 58;
                                          label = [ "1" ];
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
                                        { id = 62; content = Whitespace " " };
                                      Secondary
                                        { id = 59; content = Whitespace " " };
                                      Secondary
                                        {
                                          id = 60;
                                          content = Whitespace "\226\143\142";
                                        };
                                    ];
                                  ];
                              };
                            Secondary { id = 66; content = Whitespace " " };
                            Tile
                              {
                                id = 69;
                                label = [ "fib" ];
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
                                id = 70;
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
                                          id = 72;
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
                                        { id = 73; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 74;
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
                                        { id = 76; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 77;
                                          label = [ "1" ];
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
                                    ];
                                  ];
                              };
                            Secondary { id = 78; content = Whitespace " " };
                            Tile
                              {
                                id = 79;
                                label = [ "+" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 17; sort = Exp },
                                        { shape = Concave 17; sort = Exp } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 81; content = Whitespace " " };
                            Tile
                              {
                                id = 84;
                                label = [ "fib" ];
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
                                id = 85;
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
                                          id = 87;
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
                                        { id = 88; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 89;
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
                                        { id = 91; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 92;
                                          label = [ "2" ];
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
                                    ];
                                  ];
                              };
                            Secondary { id = 96; content = Whitespace " " };
                            Secondary { id = 93; content = Whitespace " " };
                            Secondary
                              { id = 94; content = Whitespace "\226\143\142" };
                          ];
                        ];
                    };
                  Secondary { id = 98; content = Whitespace " " };
                  Secondary { id = 99; content = Whitespace " " };
                ],
                [ Grout { id = 97; shape = Convex } ] );
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
                  ( [ Secondary { id = 100; content = Whitespace " " } ],
                    [ Grout { id = 0; shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
          };
        required = 5;
        provided = 0;
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
                      id = 104;
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
                            Secondary { id = 105; content = Whitespace " " };
                            Tile
                              {
                                id = 109;
                                label = [ "fib" ];
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
                            Secondary { id = 110; content = Whitespace " " };
                            Tile
                              {
                                id = 111;
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
                            Secondary { id = 113; content = Whitespace " " };
                            Tile
                              {
                                id = 116;
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
                            Secondary { id = 117; content = Whitespace " " };
                            Tile
                              {
                                id = 120;
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
                            Secondary { id = 121; content = Whitespace " " };
                            Tile
                              {
                                id = 124;
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
                            Secondary { id = 125; content = Whitespace " " };
                          ];
                          [
                            Secondary { id = 127; content = Whitespace " " };
                            Secondary
                              { id = 128; content = Whitespace "\226\143\142" };
                            Tile
                              {
                                id = 132;
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
                                        { id = 133; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 135;
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
                                        { id = 136; content = Whitespace " " };
                                    ];
                                  ];
                              };
                            Grout { id = 143; shape = Convex };
                            Secondary { id = 140; content = Whitespace " " };
                            Secondary
                              { id = 141; content = Whitespace "\226\143\142" };
                          ];
                        ];
                    };
                  Secondary { id = 145; content = Whitespace " " };
                  Secondary { id = 146; content = Whitespace " " };
                ],
                [ Grout { id = 144; shape = Convex } ] );
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
                            id = 150;
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
                                    { id = 151; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 155;
                                      label = [ "fib" ];
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
                                      id = 156;
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
                                    { id = 158; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 161;
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
                                    { id = 162; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 165;
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
                                    { id = 166; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 169;
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
                                    { id = 170; content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    { id = 172; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 173;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 177;
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
                                                id = 178;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 180;
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
                                                id = 181;
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 184; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 185;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 188;
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
                                                id = 189;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 191;
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
                                                id = 192;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 193;
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
                                                id = 195;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 196;
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
                                                id = 199;
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = 203;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 204;
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
                                                id = 208;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 205;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 206;
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 212; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 215;
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
                                                id = 216;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 218;
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
                                                id = 219;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 220;
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
                                                id = 222;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 223;
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
                                            Secondary
                                              {
                                                id = 226;
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = 230;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 231;
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
                                                id = 235;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 232;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 233;
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 239; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 242;
                                      label = [ "fib" ];
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
                                      id = 243;
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
                                                id = 245;
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
                                                id = 246;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 247;
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
                                                id = 249;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 250;
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
                                    { id = 251; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 252;
                                      label = [ "+" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 17; sort = Exp },
                                              { shape = Concave 17; sort = Exp }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 254; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 257;
                                      label = [ "fib" ];
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
                                      id = 258;
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
                                                id = 260;
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
                                                id = 261;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 262;
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
                                                id = 264;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 265;
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
                                  Secondary
                                    { id = 270; content = Whitespace " " };
                                  Secondary
                                    { id = 266; content = Whitespace " " };
                                  Secondary
                                    { id = 267; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 268;
                                      content = Whitespace "\226\143\142";
                                    };
                                ];
                              ];
                          };
                        Secondary { id = 272; content = Whitespace " " };
                        Secondary { id = 273; content = Whitespace " " };
                      ],
                      [ Grout { id = 271; shape = Convex } ] );
                  ancestors = [];
                };
              caret = Outer;
            };
          hint = "TODO: hint 0";
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
                            id = 277;
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
                                    { id = 278; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 282;
                                      label = [ "fib" ];
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
                                      id = 283;
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
                                    { id = 285; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 288;
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
                                    { id = 289; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 292;
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
                                    { id = 293; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 296;
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
                                    { id = 297; content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    { id = 299; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 300;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 304;
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
                                                id = 305;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 307;
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
                                                id = 308;
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 311; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 312;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 315;
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
                                                id = 316;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 318;
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
                                                id = 319;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 320;
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
                                                id = 322;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 323;
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
                                            Secondary
                                              {
                                                id = 326;
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = 330;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 331;
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
                                                id = 336;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 332;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 333;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 334;
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 340; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 343;
                                      label = [ "fib" ];
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
                                      id = 344;
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
                                                id = 346;
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
                                                id = 347;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 348;
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
                                                id = 350;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 351;
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
                                  Secondary
                                    { id = 352; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 353;
                                      label = [ "+" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 17; sort = Exp },
                                              { shape = Concave 17; sort = Exp }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 355; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 358;
                                      label = [ "fib" ];
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
                                      id = 359;
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
                                                id = 361;
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
                                                id = 362;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 363;
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
                                                id = 365;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 366;
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
                                  Secondary
                                    { id = 370; content = Whitespace " " };
                                  Secondary
                                    { id = 367; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 368;
                                      content = Whitespace "\226\143\142";
                                    };
                                ];
                              ];
                          };
                        Secondary { id = 372; content = Whitespace " " };
                        Secondary { id = 373; content = Whitespace " " };
                      ],
                      [ Grout { id = 371; shape = Convex } ] );
                  ancestors = [];
                };
              caret = Outer;
            };
          hint = "TODO: hint 1";
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
                          id = 378;
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
                                Secondary { id = 379; content = Whitespace " " };
                                Tile
                                  {
                                    id = 383;
                                    label = [ "fib" ];
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
                                    id = 384;
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
                                              id = 386;
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
                                Secondary { id = 387; content = Whitespace " " };
                                Tile
                                  {
                                    id = 390;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 13; sort = Exp },
                                            { shape = Concave 13; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 391; content = Whitespace " " };
                                Tile
                                  {
                                    id = 392;
                                    label = [ "1" ];
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
                                Secondary { id = 395; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 397;
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
                        { id = 399; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 404;
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
                                Secondary { id = 405; content = Whitespace " " };
                                Tile
                                  {
                                    id = 409;
                                    label = [ "fib" ];
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
                                    id = 410;
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
                                              id = 412;
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
                                Secondary { id = 413; content = Whitespace " " };
                                Tile
                                  {
                                    id = 416;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 13; sort = Exp },
                                            { shape = Concave 13; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 417; content = Whitespace " " };
                                Tile
                                  {
                                    id = 418;
                                    label = [ "1" ];
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
                                Secondary { id = 421; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 423;
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
                        { id = 425; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 430;
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
                                Secondary { id = 431; content = Whitespace " " };
                                Tile
                                  {
                                    id = 435;
                                    label = [ "fib" ];
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
                                    id = 436;
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
                                              id = 438;
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
                                Secondary { id = 439; content = Whitespace " " };
                                Tile
                                  {
                                    id = 442;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 13; sort = Exp },
                                            { shape = Concave 13; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 443; content = Whitespace " " };
                                Tile
                                  {
                                    id = 444;
                                    label = [ "2" ];
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
                                Secondary { id = 447; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 449;
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
                        { id = 451; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 456;
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
                                Secondary { id = 457; content = Whitespace " " };
                                Tile
                                  {
                                    id = 461;
                                    label = [ "fib" ];
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
                                    id = 462;
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
                                              id = 464;
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
                                Secondary { id = 465; content = Whitespace " " };
                                Tile
                                  {
                                    id = 468;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 13; sort = Exp },
                                            { shape = Concave 13; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 469; content = Whitespace " " };
                                Tile
                                  {
                                    id = 470;
                                    label = [ "3" ];
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
                                Secondary { id = 473; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 475;
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
                        { id = 477; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 482;
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
                                Secondary { id = 483; content = Whitespace " " };
                                Tile
                                  {
                                    id = 487;
                                    label = [ "fib" ];
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
                                    id = 488;
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
                                              id = 490;
                                              label = [ "4" ];
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
                                Secondary { id = 491; content = Whitespace " " };
                                Tile
                                  {
                                    id = 494;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 13; sort = Exp },
                                            { shape = Concave 13; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 495; content = Whitespace " " };
                                Tile
                                  {
                                    id = 496;
                                    label = [ "5" ];
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
                                Secondary { id = 499; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 501;
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
                        { id = 503; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 508;
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
                                Secondary { id = 509; content = Whitespace " " };
                                Tile
                                  {
                                    id = 513;
                                    label = [ "fib" ];
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
                                    id = 514;
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
                                              id = 516;
                                              label = [ "5" ];
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
                                Secondary { id = 517; content = Whitespace " " };
                                Tile
                                  {
                                    id = 520;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 13; sort = Exp },
                                            { shape = Concave 13; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 521; content = Whitespace " " };
                                Tile
                                  {
                                    id = 522;
                                    label = [ "8" ];
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
                                Secondary { id = 525; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 527;
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
                        { id = 529; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 534;
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
                                Secondary { id = 535; content = Whitespace " " };
                                Tile
                                  {
                                    id = 539;
                                    label = [ "fib" ];
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
                                    id = 540;
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
                                              id = 542;
                                              label = [ "6" ];
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
                                Secondary { id = 543; content = Whitespace " " };
                                Tile
                                  {
                                    id = 546;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 13; sort = Exp },
                                            { shape = Concave 13; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 547; content = Whitespace " " };
                                Tile
                                  {
                                    id = 549;
                                    label = [ "13" ];
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
                                Secondary { id = 552; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 554;
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
                        { id = 556; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 561;
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
                                Secondary { id = 562; content = Whitespace " " };
                                Tile
                                  {
                                    id = 566;
                                    label = [ "fib" ];
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
                                    id = 567;
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
                                              id = 569;
                                              label = [ "7" ];
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
                                Secondary { id = 570; content = Whitespace " " };
                                Tile
                                  {
                                    id = 573;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 13; sort = Exp },
                                            { shape = Concave 13; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 574; content = Whitespace " " };
                                Tile
                                  {
                                    id = 576;
                                    label = [ "21" ];
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
                                Secondary { id = 579; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 581;
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
                        { id = 583; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 588;
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
                                Secondary { id = 589; content = Whitespace " " };
                                Tile
                                  {
                                    id = 593;
                                    label = [ "fib" ];
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
                                    id = 594;
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
                                              id = 596;
                                              label = [ "8" ];
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
                                Secondary { id = 597; content = Whitespace " " };
                                Tile
                                  {
                                    id = 600;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 13; sort = Exp },
                                            { shape = Concave 13; sort = Exp }
                                          );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 601; content = Whitespace " " };
                                Tile
                                  {
                                    id = 603;
                                    label = [ "34" ];
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
                                Secondary { id = 606; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 608;
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
                        { id = 610; content = Whitespace "\226\143\142" };
                      Secondary { id = 611; content = Whitespace " " };
                    ],
                    [ Grout { id = 609; shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
          };
        hints = [];
      };
    syntax_tests =
      [ ("fib is recursive", Haz3lschool.SyntaxTest.is_recursive "fib") ];
  }
