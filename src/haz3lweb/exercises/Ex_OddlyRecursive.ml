let prompt = Ex_OddlyRecursive_prompt.prompt
let id : int -> Haz3lcore.Id.t = Haz3lcore.Id.mk Default

let exercise : SchoolExercise.spec =
  {
    next_id = id 5134;
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
                  Whitespace { id = id 4466; content = "\226\143\142" };
                  Tile
                    {
                      id = id 4470;
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
                            Whitespace { id = id 4471; content = " " };
                            Tile
                              {
                                id = id 4474;
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
                            Whitespace { id = id 4475; content = " " };
                          ];
                        ];
                    };
                  Whitespace { id = id 4478; content = "\226\143\142" };
                  Tile
                    {
                      id = id 4481;
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
                            Whitespace { id = id 4482; content = " " };
                            Tile
                              {
                                id = id 4485;
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
                            Whitespace { id = id 4488; content = " " };
                          ];
                          [
                            Whitespace { id = id 4492; content = " " };
                            Tile
                              {
                                id = id 4498;
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
                            Whitespace { id = id 4501; content = " " };
                          ];
                        ];
                    };
                  Whitespace { id = id 4505; content = " " };
                  Tile
                    {
                      id = id 4510;
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
                  Whitespace { id = id 4513; content = " " };
                  Whitespace { id = id 5062; content = "\226\143\142" };
                ],
                [] );
            ancestors =
              [
                ( {
                    id = id 4436;
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
                            Whitespace { id = id 4437; content = " " };
                            Tile
                              {
                                id = id 4442;
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
                            Whitespace { id = id 4443; content = " " };
                            Tile
                              {
                                id = id 4446;
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
                            Whitespace { id = id 4447; content = " " };
                            Tile
                              {
                                id = id 4452;
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
                            Whitespace { id = id 4453; content = " " };
                            Tile
                              {
                                id = id 4457;
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
                            Whitespace { id = id 4458; content = " " };
                            Tile
                              {
                                id = id 4463;
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
                            Whitespace { id = id 4464; content = " " };
                          ];
                        ],
                        [] );
                  },
                  ( [],
                    [
                      Whitespace { id = id 4515; content = " " };
                      Grout { id = id 4514; shape = Convex };
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
                  Whitespace { id = id 4544; content = "\226\143\142" };
                  Tile
                    {
                      id = id 4548;
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
                            Whitespace { id = id 4549; content = " " };
                            Tile
                              {
                                id = id 4552;
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
                            Whitespace { id = id 4553; content = " " };
                          ];
                        ];
                    };
                  Whitespace { id = id 4556; content = "\226\143\142" };
                  Tile
                    {
                      id = id 4559;
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
                            Whitespace { id = id 4560; content = " " };
                            Tile
                              {
                                id = id 4563;
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
                            Whitespace { id = id 4564; content = " " };
                            Tile
                              {
                                id = id 4567;
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
                            Whitespace { id = id 4568; content = " " };
                            Tile
                              {
                                id = id 4570;
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
                            Whitespace { id = id 4573; content = " " };
                            Whitespace
                              { id = id 4571; content = "\226\143\142" };
                          ];
                          [
                            Whitespace { id = id 4577; content = " " };
                            Tile
                              {
                                id = id 4581;
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
                                id = id 4582;
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
                                          id = id 4585;
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
                                          id = id 4587;
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
                            Whitespace { id = id 4590; content = " " };
                            Whitespace
                              { id = id 4588; content = "\226\143\142" };
                          ];
                        ];
                    };
                  Whitespace { id = id 4594; content = " " };
                  Tile
                    {
                      id = id 4597;
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
                            Whitespace { id = id 4598; content = " " };
                            Tile
                              {
                                id = id 4601;
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
                            Whitespace { id = id 4602; content = " " };
                            Tile
                              {
                                id = id 4606;
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
                            Whitespace { id = id 4607; content = " " };
                            Tile
                              {
                                id = id 4609;
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
                            Whitespace { id = id 4612; content = " " };
                          ];
                          [
                            Whitespace { id = id 4616; content = " " };
                            Tile
                              {
                                id = id 4622;
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
                            Whitespace { id = id 4625; content = " " };
                            Whitespace
                              { id = id 4623; content = "\226\143\142" };
                          ];
                        ];
                    };
                  Whitespace { id = id 4629; content = " " };
                  Tile
                    {
                      id = id 4633;
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
                      id = id 4634;
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
                                id = id 4639;
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
                                id = id 4640;
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
                                          id = id 4643;
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
                                          id = id 4646;
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
                                          id = id 4648;
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
                  Whitespace { id = id 4651; content = " " };
                  Whitespace { id = id 5063; content = "\226\143\142" };
                ],
                [] );
            ancestors =
              [
                ( {
                    id = id 4519;
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
                            Whitespace { id = id 4520; content = " " };
                            Tile
                              {
                                id = id 4525;
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
                                id = id 4528;
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
                                id = id 4532;
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
                                id = id 4536;
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
                                id = id 4541;
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
                            Whitespace { id = id 4542; content = " " };
                          ];
                        ],
                        [] );
                  },
                  ( [],
                    [
                      Whitespace { id = id 4653; content = " " };
                      Grout { id = id 4652; shape = Convex };
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
                          id = id 225;
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
                                Whitespace { id = id 226; content = " " };
                                Tile
                                  {
                                    id = id 231;
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
                                    id = id 232;
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
                                              id = id 239;
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
                                Whitespace { id = id 242; content = " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = id 246;
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
                      Whitespace { id = id 247; content = "\226\143\142" };
                      Tile
                        {
                          id = id 252;
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
                                Whitespace { id = id 253; content = " " };
                                Tile
                                  {
                                    id = id 258;
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
                                    id = id 259;
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
                                              id = id 264;
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
                                              id = id 265;
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
                                                        id = id 271;
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
                                Whitespace { id = id 274; content = " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = id 278;
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
                      Whitespace { id = id 279; content = " " };
                      Whitespace { id = id 5065; content = "\226\143\142" };
                    ],
                    [ Grout { id = id 5064; shape = Convex } ] );
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
                      id = id 283;
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
                            Whitespace { id = id 284; content = " " };
                            Tile
                              {
                                id = id 289;
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
                                id = id 292;
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
                            Whitespace { id = id 293; content = " " };
                            Tile
                              {
                                id = id 297;
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
                            Whitespace { id = id 298; content = " " };
                            Tile
                              {
                                id = id 302;
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
                            Whitespace { id = id 303; content = " " };
                            Tile
                              {
                                id = id 308;
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
                            Whitespace { id = id 309; content = " " };
                          ];
                          [
                            Whitespace { id = id 311; content = "\226\143\142" };
                            Tile
                              {
                                id = id 315;
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
                                      Whitespace { id = id 316; content = " " };
                                      Tile
                                        {
                                          id = id 319;
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
                                      Whitespace { id = id 320; content = " " };
                                    ];
                                  ];
                              };
                            Whitespace { id = id 5067; content = " " };
                            Grout { id = id 325; shape = Convex };
                            Whitespace { id = id 323; content = " " };
                            Whitespace
                              { id = id 5068; content = "\226\143\142" };
                          ];
                        ];
                    };
                  Whitespace { id = id 5069; content = " " };
                ],
                [ Grout { id = id 326; shape = Convex } ] );
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
                            id = id 4657;
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
                                  Whitespace { id = id 4658; content = " " };
                                  Tile
                                    {
                                      id = id 4663;
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
                                      id = id 4666;
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
                                  Whitespace { id = id 4667; content = " " };
                                  Tile
                                    {
                                      id = id 4671;
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
                                  Whitespace { id = id 4672; content = " " };
                                  Tile
                                    {
                                      id = id 4676;
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
                                  Whitespace { id = id 4677; content = " " };
                                  Tile
                                    {
                                      id = id 4682;
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
                                  Whitespace { id = id 4683; content = " " };
                                ];
                                [
                                  Whitespace
                                    { id = id 4685; content = "\226\143\142" };
                                  Tile
                                    {
                                      id = id 4689;
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
                                            Whitespace
                                              { id = id 4690; content = " " };
                                            Tile
                                              {
                                                id = id 4693;
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
                                            Whitespace
                                              { id = id 4694; content = " " };
                                          ];
                                        ];
                                    };
                                  Whitespace { id = id 4697; content = " " };
                                  Tile
                                    {
                                      id = id 4703;
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
                                  Whitespace { id = id 4706; content = " " };
                                  Whitespace
                                    { id = id 4704; content = "\226\143\142" };
                                ];
                              ];
                          };
                        Whitespace { id = id 4708; content = " " };
                      ],
                      [ Grout { id = id 4707; shape = Convex } ] );
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
                            id = id 4712;
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
                                  Whitespace { id = id 4713; content = " " };
                                  Tile
                                    {
                                      id = id 4718;
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
                                      id = id 4721;
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
                                  Whitespace { id = id 4722; content = " " };
                                  Tile
                                    {
                                      id = id 4726;
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
                                  Whitespace { id = id 4727; content = " " };
                                  Tile
                                    {
                                      id = id 4731;
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
                                  Whitespace { id = id 4732; content = " " };
                                  Tile
                                    {
                                      id = id 4737;
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
                                  Whitespace { id = id 4738; content = " " };
                                ];
                                [
                                  Whitespace
                                    { id = id 4740; content = "\226\143\142" };
                                  Tile
                                    {
                                      id = id 4744;
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
                                            Whitespace
                                              { id = id 4745; content = " " };
                                            Tile
                                              {
                                                id = id 4748;
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
                                            Whitespace
                                              { id = id 4749; content = " " };
                                          ];
                                        ];
                                    };
                                  Whitespace { id = id 4752; content = " " };
                                  Tile
                                    {
                                      id = id 4757;
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
                                  Whitespace { id = id 4760; content = " " };
                                  Whitespace
                                    { id = id 4758; content = "\226\143\142" };
                                ];
                              ];
                          };
                        Whitespace { id = id 5066; content = " " };
                      ],
                      [ Grout { id = id 4761; shape = Convex } ] );
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
                            id = id 4765;
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
                                  Whitespace { id = id 4766; content = " " };
                                  Tile
                                    {
                                      id = id 4771;
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
                                      id = id 4774;
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
                                  Whitespace { id = id 4775; content = " " };
                                  Tile
                                    {
                                      id = id 4779;
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
                                  Whitespace { id = id 4780; content = " " };
                                  Tile
                                    {
                                      id = id 4784;
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
                                  Whitespace { id = id 4785; content = " " };
                                  Tile
                                    {
                                      id = id 4790;
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
                                  Whitespace { id = id 4791; content = " " };
                                ];
                                [
                                  Whitespace
                                    { id = id 4793; content = "\226\143\142" };
                                  Tile
                                    {
                                      id = id 4797;
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
                                            Whitespace
                                              { id = id 4798; content = " " };
                                            Tile
                                              {
                                                id = id 4801;
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
                                            Whitespace
                                              { id = id 4802; content = " " };
                                          ];
                                        ];
                                    };
                                  Whitespace { id = id 4805; content = " " };
                                  Tile
                                    {
                                      id = id 4808;
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
                                            Whitespace
                                              { id = id 4809; content = " " };
                                            Tile
                                              {
                                                id = id 4812;
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
                                            Whitespace
                                              { id = id 4813; content = " " };
                                            Tile
                                              {
                                                id = id 4816;
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
                                            Whitespace
                                              { id = id 4817; content = " " };
                                            Tile
                                              {
                                                id = id 4819;
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
                                            Whitespace
                                              { id = id 4822; content = " " };
                                          ];
                                          [
                                            Whitespace
                                              { id = id 4826; content = " " };
                                            Tile
                                              {
                                                id = id 4830;
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
                                                id = id 4831;
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
                                                          id = id 4834;
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
                                                          id = id 4836;
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
                                            Whitespace
                                              { id = id 4839; content = " " };
                                            Whitespace
                                              {
                                                id = id 4837;
                                                content = "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Whitespace { id = id 4843; content = " " };
                                  Tile
                                    {
                                      id = id 4846;
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
                                            Whitespace
                                              { id = id 4847; content = " " };
                                            Tile
                                              {
                                                id = id 4850;
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
                                            Whitespace
                                              { id = id 4851; content = " " };
                                            Tile
                                              {
                                                id = id 4855;
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
                                            Whitespace
                                              { id = id 4856; content = " " };
                                            Tile
                                              {
                                                id = id 4858;
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
                                            Whitespace
                                              { id = id 4861; content = " " };
                                          ];
                                          [
                                            Whitespace
                                              { id = id 4865; content = " " };
                                            Tile
                                              {
                                                id = id 4870;
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
                                            Whitespace
                                              { id = id 4873; content = " " };
                                            Whitespace
                                              {
                                                id = id 4871;
                                                content = "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Whitespace { id = id 4877; content = " " };
                                  Tile
                                    {
                                      id = id 4880;
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
                                            Whitespace
                                              { id = id 4881; content = " " };
                                            Tile
                                              {
                                                id = id 4884;
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
                                            Whitespace
                                              { id = id 4885; content = " " };
                                            Tile
                                              {
                                                id = id 4889;
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
                                            Whitespace
                                              { id = id 4890; content = " " };
                                            Tile
                                              {
                                                id = id 4892;
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
                                            Whitespace
                                              { id = id 4895; content = " " };
                                          ];
                                          [
                                            Whitespace
                                              { id = id 4899; content = " " };
                                            Tile
                                              {
                                                id = id 4904;
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
                                            Whitespace
                                              { id = id 4907; content = " " };
                                            Whitespace
                                              {
                                                id = id 4905;
                                                content = "\226\143\142";
                                              };
                                          ];
                                        ];
                                    };
                                  Whitespace { id = id 4911; content = " " };
                                  Tile
                                    {
                                      id = id 4915;
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
                                      id = id 4916;
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
                                                id = id 4919;
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
                                            Whitespace
                                              { id = id 4920; content = " " };
                                            Tile
                                              {
                                                id = id 4923;
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
                                            Whitespace
                                              { id = id 4924; content = " " };
                                            Tile
                                              {
                                                id = id 4926;
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
                                  Whitespace { id = id 4929; content = " " };
                                ];
                              ];
                          };
                        Whitespace { id = id 4931; content = "\226\143\142" };
                      ],
                      [ Grout { id = id 4930; shape = Convex } ] );
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
                          id = id 4936;
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
                                Whitespace { id = id 4937; content = " " };
                                Tile
                                  {
                                    id = id 4942;
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
                                    id = id 4943;
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
                                              id = id 4948;
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
                                              id = id 4949;
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
                                                        id = id 4952;
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
                                Whitespace { id = id 4955; content = " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = id 4959;
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
                      Whitespace { id = id 4960; content = "\226\143\142" };
                      Tile
                        {
                          id = id 4965;
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
                                Whitespace { id = id 4966; content = " " };
                                Tile
                                  {
                                    id = id 4971;
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
                                    id = id 4972;
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
                                              id = id 4975;
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
                                Whitespace { id = id 4978; content = " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = id 4982;
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
                      Whitespace { id = id 4983; content = "\226\143\142" };
                      Tile
                        {
                          id = id 4988;
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
                                Whitespace { id = id 4989; content = " " };
                                Tile
                                  {
                                    id = id 4994;
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
                                    id = id 4995;
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
                                              id = id 5000;
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
                                              id = id 5001;
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
                                                        id = id 5004;
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
                                Whitespace { id = id 5007; content = " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = id 5070;
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
                      Whitespace { id = id 5071; content = "\226\143\142" };
                      Tile
                        {
                          id = id 5077;
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
                                Whitespace { id = id 5078; content = " " };
                                Tile
                                  {
                                    id = id 5083;
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
                                    id = id 5084;
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
                                              id = id 5087;
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
                                Whitespace { id = id 5090; content = " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = id 5092;
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
                      Whitespace { id = id 5012; content = "\226\143\142" };
                      Tile
                        {
                          id = id 5017;
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
                                Whitespace { id = id 5018; content = " " };
                                Tile
                                  {
                                    id = id 5023;
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
                                    id = id 5024;
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
                                              id = id 5029;
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
                                              id = id 5030;
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
                                                        id = id 5034;
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
                                Whitespace { id = id 5037; content = " " };
                              ];
                            ];
                        };
                      Tile
                        {
                          id = id 5095;
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
                      Whitespace { id = id 5096; content = " " };
                      Whitespace { id = id 5097; content = "\226\143\142" };
                      Tile
                        {
                          id = id 5102;
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
                                Whitespace { id = id 5103; content = " " };
                                Tile
                                  {
                                    id = id 5133;
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
                                    id = id 5115;
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
                                              id = id 5129;
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
                                Whitespace { id = id 5121; content = " " };
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
  }
