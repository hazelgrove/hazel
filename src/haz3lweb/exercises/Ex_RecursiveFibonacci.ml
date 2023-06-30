let prompt = Ex_RecursiveFibonacci_prompt.prompt

let exercise : SchoolExercise.spec =
  {
    next_id = 813;
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
            siblings = ([ Grout { id = 0; shape = Convex } ], []);
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
                  Secondary { id = 38; content = Whitespace " " };
                  Secondary { id = 39; content = Whitespace "\226\143\142" };
                  Tile
                    {
                      id = 43;
                      label = [ "fun"; "->" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 14; sort = Exp } );
                        };
                      shards = [ 0; 1 ];
                      children =
                        [
                          [
                            Secondary { id = 44; content = Whitespace " " };
                            Tile
                              {
                                id = 47;
                                label = [ "x" ];
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
                            Secondary { id = 48; content = Whitespace " " };
                          ];
                        ];
                    };
                  Secondary { id = 51; content = Whitespace " " };
                  Secondary { id = 52; content = Whitespace "\226\143\142" };
                  Tile
                    {
                      id = 55;
                      label = [ "if"; "then"; "else" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Exp; Exp ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 12; sort = Exp } );
                        };
                      shards = [ 0; 1; 2 ];
                      children =
                        [
                          [
                            Secondary { id = 56; content = Whitespace " " };
                            Tile
                              {
                                id = 59;
                                label = [ "x" ];
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
                            Secondary { id = 60; content = Whitespace " " };
                            Tile
                              {
                                id = 63;
                                label = [ "<" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 5; sort = Exp },
                                        { shape = Concave 5; sort = Exp } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 64; content = Whitespace " " };
                            Tile
                              {
                                id = 66;
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
                            Secondary { id = 69; content = Whitespace " " };
                          ];
                          [
                            Secondary { id = 73; content = Whitespace " " };
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
                                        { shape = Convex; sort = Exp } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 80; content = Whitespace " " };
                            Secondary
                              { id = 78; content = Whitespace "\226\143\142" };
                          ];
                        ];
                    };
                  Secondary { id = 84; content = Whitespace " " };
                  Tile
                    {
                      id = 88;
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
                      id = 89;
                      label = [ "("; ")" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Exp ];
                          nibs =
                            ( { shape = Concave 1; sort = Exp },
                              { shape = Convex; sort = Exp } );
                        };
                      shards = [ 0; 1 ];
                      children =
                        [
                          [
                            Tile
                              {
                                id = 92;
                                label = [ "x" ];
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
                            Secondary { id = 93; content = Whitespace " " };
                            Tile
                              {
                                id = 96;
                                label = [ "-" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 4; sort = Exp },
                                        { shape = Concave 4; sort = Exp } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 97; content = Whitespace " " };
                            Tile
                              {
                                id = 99;
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
                          ];
                        ];
                    };
                  Secondary { id = 100; content = Whitespace " " };
                  Tile
                    {
                      id = 103;
                      label = [ "+" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 4; sort = Exp },
                              { shape = Concave 4; sort = Exp } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Secondary { id = 104; content = Whitespace " " };
                  Tile
                    {
                      id = 108;
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
                      id = 109;
                      label = [ "("; ")" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Exp ];
                          nibs =
                            ( { shape = Concave 1; sort = Exp },
                              { shape = Convex; sort = Exp } );
                        };
                      shards = [ 0; 1 ];
                      children =
                        [
                          [
                            Tile
                              {
                                id = 112;
                                label = [ "x" ];
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
                            Secondary { id = 113; content = Whitespace " " };
                            Tile
                              {
                                id = 116;
                                label = [ "-" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 4; sort = Exp },
                                        { shape = Concave 4; sort = Exp } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 117; content = Whitespace " " };
                            Tile
                              {
                                id = 119;
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
                          ];
                        ];
                    };
                  Secondary { id = 122; content = Whitespace " " };
                ],
                [ Secondary { id = 126; content = Whitespace "\226\143\142" } ]
              );
            ancestors =
              [
                ( {
                    id = 11;
                    label = [ "let"; "="; "in" ];
                    mold =
                      {
                        out = Exp;
                        in_ = [ Pat; Exp ];
                        nibs =
                          ( { shape = Convex; sort = Exp },
                            { shape = Concave 14; sort = Exp } );
                      };
                    shards = ([ 0; 1 ], [ 2 ]);
                    children =
                      ( [
                          [
                            Secondary { id = 12; content = Whitespace " " };
                            Tile
                              {
                                id = 17;
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
                                id = 20;
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
                            Secondary { id = 21; content = Whitespace " " };
                            Tile
                              {
                                id = 25;
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
                            Secondary { id = 26; content = Whitespace " " };
                            Tile
                              {
                                id = 30;
                                label = [ "->" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 6; sort = Typ },
                                        { shape = Concave 6; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 31; content = Whitespace " " };
                            Tile
                              {
                                id = 35;
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
                            Secondary { id = 36; content = Whitespace " " };
                          ];
                        ],
                        [] );
                  },
                  ( [],
                    [
                      Secondary { id = 124; content = Whitespace " " };
                      Grout { id = 123; shape = Convex };
                    ] ) );
              ];
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
                siblings = ([], [ Grout { id = 2; shape = Convex } ]);
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
                      id = 472;
                      label = [ "let"; "="; "in" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat; Exp ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 14; sort = Exp } );
                        };
                      shards = [ 0; 1; 2 ];
                      children =
                        [
                          [
                            Secondary { id = 473; content = Whitespace " " };
                            Tile
                              {
                                id = 478;
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
                            Secondary { id = 479; content = Whitespace " " };
                            Tile
                              {
                                id = 482;
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
                            Secondary { id = 483; content = Whitespace " " };
                            Tile
                              {
                                id = 487;
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
                            Secondary { id = 488; content = Whitespace " " };
                            Tile
                              {
                                id = 492;
                                label = [ "->" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 6; sort = Typ },
                                        { shape = Concave 6; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary { id = 493; content = Whitespace " " };
                            Tile
                              {
                                id = 497;
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
                            Secondary { id = 498; content = Whitespace " " };
                          ];
                          [
                            Secondary { id = 500; content = Whitespace " " };
                            Secondary
                              { id = 501; content = Whitespace "\226\143\142" };
                            Tile
                              {
                                id = 505;
                                label = [ "fun"; "->" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [ Pat ];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Concave 14; sort = Exp } );
                                  };
                                shards = [ 0; 1 ];
                                children =
                                  [
                                    [
                                      Secondary
                                        { id = 506; content = Whitespace " " };
                                      Tile
                                        {
                                          id = 509;
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
                                        { id = 510; content = Whitespace " " };
                                    ];
                                  ];
                              };
                            Secondary { id = 521; content = Whitespace " " };
                            Grout { id = 518; shape = Convex };
                            Secondary
                              { id = 516; content = Whitespace "\226\143\142" };
                          ];
                        ];
                    };
                  Secondary { id = 520; content = Whitespace " " };
                  Grout { id = 519; shape = Convex };
                ],
                [] );
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
                            id = 131;
                            label = [ "let"; "="; "in" ];
                            mold =
                              {
                                out = Exp;
                                in_ = [ Pat; Exp ];
                                nibs =
                                  ( { shape = Convex; sort = Exp },
                                    { shape = Concave 14; sort = Exp } );
                              };
                            shards = [ 0; 1; 2 ];
                            children =
                              [
                                [
                                  Secondary
                                    { id = 132; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 137;
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
                                      id = 140;
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
                                    { id = 141; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 145;
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
                                    { id = 146; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 150;
                                      label = [ "->" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 6; sort = Typ },
                                              { shape = Concave 6; sort = Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 151; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 155;
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
                                    { id = 156; content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    { id = 158; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 159;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 163;
                                      label = [ "fun"; "->" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Pat ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 14; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 164;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 167;
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
                                                id = 168;
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 173; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 174;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 177;
                                      label = [ "if"; "then"; "else" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp; Exp ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 12; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1; 2 ];
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
                                                id = 181;
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
                                                id = 182;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 185;
                                                label = [ "<" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 5;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 5;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 186;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 188;
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
                                                id = 191;
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = 195;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 197;
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
                                                id = 200;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 198;
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 204; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 219;
                                      label = [ "if"; "then"; "else" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp; Exp ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 12; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1; 2 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 220;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 223;
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
                                                id = 224;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 227;
                                                label = [ "<" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 5;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 5;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 228;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 230;
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
                                                id = 233;
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = 237;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 239;
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
                                                id = 242;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 240;
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 246; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 250;
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
                                      id = 251;
                                      label = [ "("; ")" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp ];
                                          nibs =
                                            ( { shape = Concave 1; sort = Exp },
                                              { shape = Convex; sort = Exp } );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Tile
                                              {
                                                id = 257;
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
                                                id = 258;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 261;
                                                label = [ "-" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 4;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 4;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 262;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 264;
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
                                    { id = 265; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 268;
                                      label = [ "+" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 4; sort = Exp },
                                              { shape = Concave 4; sort = Exp }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 269; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 273;
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
                                      id = 274;
                                      label = [ "("; ")" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp ];
                                          nibs =
                                            ( { shape = Concave 1; sort = Exp },
                                              { shape = Convex; sort = Exp } );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Tile
                                              {
                                                id = 277;
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
                                                id = 278;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 281;
                                                label = [ "-" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 4;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 4;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 282;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 284;
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
                                    { id = 288; content = Whitespace " " };
                                  Secondary
                                    { id = 285; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 286;
                                      content = Whitespace "\226\143\142";
                                    };
                                ];
                              ];
                          };
                        Secondary { id = 290; content = Whitespace " " };
                      ],
                      [ Grout { id = 289; shape = Convex } ] );
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
                            id = 295;
                            label = [ "let"; "="; "in" ];
                            mold =
                              {
                                out = Exp;
                                in_ = [ Pat; Exp ];
                                nibs =
                                  ( { shape = Convex; sort = Exp },
                                    { shape = Concave 14; sort = Exp } );
                              };
                            shards = [ 0; 1; 2 ];
                            children =
                              [
                                [
                                  Secondary
                                    { id = 296; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 301;
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
                                      id = 304;
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
                                    { id = 305; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 309;
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
                                    { id = 310; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 314;
                                      label = [ "->" ];
                                      mold =
                                        {
                                          out = Typ;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 6; sort = Typ },
                                              { shape = Concave 6; sort = Typ }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 315; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 319;
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
                                    { id = 320; content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    { id = 322; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 323;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 327;
                                      label = [ "fun"; "->" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Pat ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 14; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 328;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 331;
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
                                                id = 332;
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 335; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 336;
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = 339;
                                      label = [ "if"; "then"; "else" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp; Exp ];
                                          nibs =
                                            ( { shape = Convex; sort = Exp },
                                              { shape = Concave 12; sort = Exp }
                                            );
                                        };
                                      shards = [ 0; 1; 2 ];
                                      children =
                                        [
                                          [
                                            Secondary
                                              {
                                                id = 340;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 343;
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
                                                id = 344;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 347;
                                                label = [ "<" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 5;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 5;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 348;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 350;
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
                                                id = 353;
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = 357;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 359;
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
                                                id = 363;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 360;
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = 361;
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = 409; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 415;
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
                                      id = 416;
                                      label = [ "("; ")" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp ];
                                          nibs =
                                            ( { shape = Concave 1; sort = Exp },
                                              { shape = Convex; sort = Exp } );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Tile
                                              {
                                                id = 419;
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
                                                id = 420;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 423;
                                                label = [ "-" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 4;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 4;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 424;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 440;
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
                                    { id = 441; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 445;
                                      label = [ "+" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [];
                                          nibs =
                                            ( { shape = Concave 4; sort = Exp },
                                              { shape = Concave 4; sort = Exp }
                                            );
                                        };
                                      shards = [ 0 ];
                                      children = [];
                                    };
                                  Secondary
                                    { id = 446; content = Whitespace " " };
                                  Tile
                                    {
                                      id = 450;
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
                                      id = 451;
                                      label = [ "("; ")" ];
                                      mold =
                                        {
                                          out = Exp;
                                          in_ = [ Exp ];
                                          nibs =
                                            ( { shape = Concave 1; sort = Exp },
                                              { shape = Convex; sort = Exp } );
                                        };
                                      shards = [ 0; 1 ];
                                      children =
                                        [
                                          [
                                            Tile
                                              {
                                                id = 454;
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
                                                id = 455;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 458;
                                                label = [ "-" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 4;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 4;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = 459;
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = 461;
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
                                    { id = 464; content = Whitespace " " };
                                  Secondary
                                    {
                                      id = 466;
                                      content = Whitespace "\226\143\142";
                                    };
                                ];
                              ];
                          };
                        Secondary { id = 467; content = Whitespace " " };
                      ],
                      [ Grout { id = 465; shape = Convex } ] );
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
                          id = 528;
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
                                Secondary { id = 529; content = Whitespace " " };
                                Tile
                                  {
                                    id = 534;
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
                                    id = 535;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 1; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 538;
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
                                Secondary { id = 539; content = Whitespace " " };
                                Tile
                                  {
                                    id = 543;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 544; content = Whitespace " " };
                                Tile
                                  {
                                    id = 546;
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
                                Secondary { id = 549; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 553;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 554; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 559;
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
                                Secondary { id = 560; content = Whitespace " " };
                                Tile
                                  {
                                    id = 565;
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
                                    id = 566;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 1; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 569;
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
                                Secondary { id = 570; content = Whitespace " " };
                                Tile
                                  {
                                    id = 574;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 575; content = Whitespace " " };
                                Tile
                                  {
                                    id = 577;
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
                                Secondary { id = 580; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 584;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 585; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 590;
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
                                Secondary { id = 591; content = Whitespace " " };
                                Tile
                                  {
                                    id = 596;
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
                                    id = 597;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 1; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 600;
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
                                Secondary { id = 601; content = Whitespace " " };
                                Tile
                                  {
                                    id = 605;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 606; content = Whitespace " " };
                                Tile
                                  {
                                    id = 612;
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
                                Secondary { id = 614; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 618;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 619; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 624;
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
                                Secondary { id = 625; content = Whitespace " " };
                                Tile
                                  {
                                    id = 630;
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
                                    id = 631;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 1; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 634;
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
                                Secondary { id = 635; content = Whitespace " " };
                                Tile
                                  {
                                    id = 639;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 640; content = Whitespace " " };
                                Tile
                                  {
                                    id = 642;
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
                                Secondary { id = 645; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 649;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 650; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 655;
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
                                Secondary { id = 656; content = Whitespace " " };
                                Tile
                                  {
                                    id = 661;
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
                                    id = 662;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 1; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 665;
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
                                Secondary { id = 666; content = Whitespace " " };
                                Tile
                                  {
                                    id = 670;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 671; content = Whitespace " " };
                                Tile
                                  {
                                    id = 683;
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
                                Secondary { id = 676; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 680;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 681; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 688;
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
                                Secondary { id = 689; content = Whitespace " " };
                                Tile
                                  {
                                    id = 694;
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
                                    id = 695;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 1; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 698;
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
                                Secondary { id = 699; content = Whitespace " " };
                                Tile
                                  {
                                    id = 703;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 704; content = Whitespace " " };
                                Tile
                                  {
                                    id = 706;
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
                                Secondary { id = 709; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 713;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 714; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 719;
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
                                Secondary { id = 720; content = Whitespace " " };
                                Tile
                                  {
                                    id = 725;
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
                                    id = 726;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 1; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 729;
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
                                Secondary { id = 730; content = Whitespace " " };
                                Tile
                                  {
                                    id = 734;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 735; content = Whitespace " " };
                                Tile
                                  {
                                    id = 738;
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
                                Secondary { id = 741; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 745;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 746; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 751;
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
                                Secondary { id = 752; content = Whitespace " " };
                                Tile
                                  {
                                    id = 757;
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
                                    id = 758;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 1; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 761;
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
                                Secondary { id = 762; content = Whitespace " " };
                                Tile
                                  {
                                    id = 766;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 767; content = Whitespace " " };
                                Tile
                                  {
                                    id = 770;
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
                                Secondary { id = 773; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 777;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 778; content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = 783;
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
                                Secondary { id = 784; content = Whitespace " " };
                                Tile
                                  {
                                    id = 789;
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
                                    id = 790;
                                    label = [ "("; ")" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp ];
                                        nibs =
                                          ( { shape = Concave 1; sort = Exp },
                                            { shape = Convex; sort = Exp } );
                                      };
                                    shards = [ 0; 1 ];
                                    children =
                                      [
                                        [
                                          Tile
                                            {
                                              id = 793;
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
                                Secondary { id = 794; content = Whitespace " " };
                                Tile
                                  {
                                    id = 798;
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 7; sort = Exp },
                                            { shape = Concave 7; sort = Exp } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary { id = 799; content = Whitespace " " };
                                Tile
                                  {
                                    id = 802;
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
                                Secondary { id = 805; content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = 811;
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 10; sort = Exp },
                                  { shape = Concave 10; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        { id = 812; content = Whitespace "\226\143\142" };
                    ],
                    [ Grout { id = 810; shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
          };
        hints = [];
        syntax_tests = { var_mention = []; recursive = [] };
      };
  }
