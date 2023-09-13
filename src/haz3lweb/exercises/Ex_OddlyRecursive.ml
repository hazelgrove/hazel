open Haz3lcore

let prompt = Ex_OddlyRecursive_prompt.prompt

let exercise : Exercise.spec =
  {
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
                  Secondary
                    { id = Id.mk (); content = Whitespace "\226\143\142" };
                  Tile
                    {
                      id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                          ];
                        ];
                    };
                  Secondary
                    { id = Id.mk (); content = Whitespace "\226\143\142" };
                  Tile
                    {
                      id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                          ];
                          [
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                          ];
                        ];
                    };
                  Secondary { id = Id.mk (); content = Whitespace " " };
                  Tile
                    {
                      id = Id.mk ();
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
                  Secondary { id = Id.mk (); content = Whitespace " " };
                  Secondary
                    { id = Id.mk (); content = Whitespace "\226\143\142" };
                ],
                [] );
            ancestors =
              [
                ( {
                    id = Id.mk ();
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
                            Secondary { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                          ];
                        ],
                        [] );
                  },
                  ( [],
                    [
                      Secondary { id = Id.mk (); content = Whitespace " " };
                      Grout { id = Id.mk (); shape = Convex };
                    ] ) );
              ];
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
                  Secondary
                    { id = Id.mk (); content = Whitespace "\226\143\142" };
                  Tile
                    {
                      id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                          ];
                        ];
                    };
                  Secondary
                    { id = Id.mk (); content = Whitespace "\226\143\142" };
                  Tile
                    {
                      id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
                                label = [ "0" ];
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
                              { id = Id.mk (); content = Whitespace " " };
                            Secondary
                              {
                                id = Id.mk ();
                                content = Whitespace "\226\143\142";
                              };
                          ];
                          [
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                                id = Id.mk ();
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
                                          id = Id.mk ();
                                          label = [ "-" ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Exp },
                                                  {
                                                    shape = Concave 2;
                                                    sort = Exp;
                                                  } );
                                            };
                                          shards = [ 0 ];
                                          children = [];
                                        };
                                      Tile
                                        {
                                          id = Id.mk ();
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
                                    ];
                                  ];
                              };
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Secondary
                              {
                                id = Id.mk ();
                                content = Whitespace "\226\143\142";
                              };
                          ];
                        ];
                    };
                  Secondary { id = Id.mk (); content = Whitespace " " };
                  Tile
                    {
                      id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
                                label = [ "0" ];
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
                              { id = Id.mk (); content = Whitespace " " };
                          ];
                          [
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                            Secondary
                              {
                                id = Id.mk ();
                                content = Whitespace "\226\143\142";
                              };
                          ];
                        ];
                    };
                  Secondary { id = Id.mk (); content = Whitespace " " };
                  Tile
                    {
                      id = Id.mk ();
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
                      id = Id.mk ();
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
                                id = Id.mk ();
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
                                id = Id.mk ();
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
                                          id = Id.mk ();
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
                                      Tile
                                        {
                                          id = Id.mk ();
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
                                      Tile
                                        {
                                          id = Id.mk ();
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
                          ];
                        ];
                    };
                  Secondary { id = Id.mk (); content = Whitespace " " };
                  Secondary
                    { id = Id.mk (); content = Whitespace "\226\143\142" };
                ],
                [] );
            ancestors =
              [
                ( {
                    id = Id.mk ();
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
                            Secondary { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                                id = Id.mk ();
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
                                id = Id.mk ();
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
                                id = Id.mk ();
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
                            Tile
                              {
                                id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                          ];
                        ],
                        [] );
                  },
                  ( [],
                    [
                      Secondary { id = Id.mk (); content = Whitespace " " };
                      Grout { id = Id.mk (); shape = Convex };
                    ] ) );
              ];
          };
        caret = Outer;
      };
    your_tests =
      {
        tests =
          {
            selection = { focus = Right; content = [] };
            backpack = [];
            relatives =
              {
                siblings =
                  ( [
                      Tile
                        {
                          id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
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
                                    id = Id.mk ();
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
                                              id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
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
                        { id = Id.mk (); content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
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
                                    id = Id.mk ();
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
                                              id = Id.mk ();
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
                                              id = Id.mk ();
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 1;
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
                                                        id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
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
                      Secondary { id = Id.mk (); content = Whitespace " " };
                      Secondary
                        { id = Id.mk (); content = Whitespace "\226\143\142" };
                    ],
                    [ Grout { id = Id.mk (); shape = Convex } ] );
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
                      id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Tile
                              {
                                id = Id.mk ();
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
                              { id = Id.mk (); content = Whitespace " " };
                          ];
                          [
                            Secondary
                              {
                                id = Id.mk ();
                                content = Whitespace "\226\143\142";
                              };
                            Tile
                              {
                                id = Id.mk ();
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
                                        {
                                          id = Id.mk ();
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id = Id.mk ();
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
                                        {
                                          id = Id.mk ();
                                          content = Whitespace " ";
                                        };
                                    ];
                                  ];
                              };
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Grout { id = Id.mk (); shape = Convex };
                            Secondary
                              { id = Id.mk (); content = Whitespace " " };
                            Secondary
                              {
                                id = Id.mk ();
                                content = Whitespace "\226\143\142";
                              };
                          ];
                        ];
                    };
                  Secondary { id = Id.mk (); content = Whitespace " " };
                ],
                [ Grout { id = Id.mk (); shape = Convex } ] );
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
                            id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    {
                                      id = Id.mk ();
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Secondary
                                    {
                                      id = Id.mk ();
                                      content = Whitespace "\226\143\142";
                                    };
                                ];
                              ];
                          };
                        Secondary { id = Id.mk (); content = Whitespace " " };
                      ],
                      [ Grout { id = Id.mk (); shape = Convex } ] );
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
                            id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    {
                                      id = Id.mk ();
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Secondary
                                    {
                                      id = Id.mk ();
                                      content = Whitespace "\226\143\142";
                                    };
                                ];
                              ];
                          };
                        Secondary { id = Id.mk (); content = Whitespace " " };
                      ],
                      [ Grout { id = Id.mk (); shape = Convex } ] );
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
                            id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                ];
                                [
                                  Secondary
                                    {
                                      id = Id.mk ();
                                      content = Whitespace "\226\143\142";
                                    };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                label = [ "("; ")" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [ Exp ];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 1;
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
                                                          id = Id.mk ();
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
                                                                      Concave 2;
                                                                    sort = Exp;
                                                                  } );
                                                            };
                                                          shards = [ 0 ];
                                                          children = [];
                                                        };
                                                      Tile
                                                        {
                                                          id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = Id.mk ();
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
                                                label = [ "==" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 7;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 7;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = Id.mk ();
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
                                                label = [ "==" ];
                                                mold =
                                                  {
                                                    out = Exp;
                                                    in_ = [];
                                                    nibs =
                                                      ( {
                                                          shape = Concave 7;
                                                          sort = Exp;
                                                        },
                                                        {
                                                          shape = Concave 7;
                                                          sort = Exp;
                                                        } );
                                                  };
                                                shards = [ 0 ];
                                                children = [];
                                              };
                                            Secondary
                                              {
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                          ];
                                          [
                                            Secondary
                                              {
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Secondary
                                              {
                                                id = Id.mk ();
                                                content =
                                                  Whitespace "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Secondary
                                    { id = Id.mk (); content = Whitespace " " };
                                  Tile
                                    {
                                      id = Id.mk ();
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
                                      id = Id.mk ();
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
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                                id = Id.mk ();
                                                content = Whitespace " ";
                                              };
                                            Tile
                                              {
                                                id = Id.mk ();
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
                                    { id = Id.mk (); content = Whitespace " " };
                                ];
                              ];
                          };
                        Secondary
                          { id = Id.mk (); content = Whitespace "\226\143\142" };
                      ],
                      [ Grout { id = Id.mk (); shape = Convex } ] );
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
                          id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
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
                                    id = Id.mk ();
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
                                              id = Id.mk ();
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
                                              id = Id.mk ();
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 1;
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
                                                        id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
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
                        { id = Id.mk (); content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
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
                                    id = Id.mk ();
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
                                              id = Id.mk ();
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
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
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
                        { id = Id.mk (); content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
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
                                    id = Id.mk ();
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
                                              id = Id.mk ();
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
                                              id = Id.mk ();
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 1;
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
                                                        id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
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
                        { id = Id.mk (); content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
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
                                    id = Id.mk ();
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
                                              id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
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
                        { id = Id.mk (); content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
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
                                    id = Id.mk ();
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
                                              id = Id.mk ();
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
                                              id = Id.mk ();
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Exp;
                                                  in_ = [ Exp ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 1;
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
                                                        id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = Id.mk ();
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
                      Secondary { id = Id.mk (); content = Whitespace " " };
                      Secondary
                        { id = Id.mk (); content = Whitespace "\226\143\142" };
                      Tile
                        {
                          id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
                                Tile
                                  {
                                    id = Id.mk ();
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
                                    id = Id.mk ();
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
                                              id = Id.mk ();
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
                                Secondary
                                  { id = Id.mk (); content = Whitespace " " };
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
