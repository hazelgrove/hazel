let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "f2a3b4c5-6789-0123-4abc-def567890123");
    title = "Fibonacci";
    version = 9;
    module_name = "Tu_Fib";
    prompt =
      "Let us partake in the time-honored tradition of introducing recursion \
       using the Fibonacci numbers. Write a function that computes the `n`th \
       Fibonacci number, where the `0`th is `0`, the `1`st is `1`, and after \
       that each number is the sum of the previous two.";
    display_hint = "Use conditionals to guard your recursion.";
    your_impl =
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
                             "52e2c074-2b84-4be3-8b88-2e3ecb65475d");
                      content = Whitespace " ";
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "d285c431-2180-450f-9d72-fed2daaab24c");
                      content = Whitespace "\n";
                    };
                ],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "a90d79d9-6228-473b-823a-8cd1a619a627");
                      shape = Convex;
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "4110616a-3f8d-4ddd-a5eb-17141ad68763");
                      content = Whitespace "\n";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "dba6eea5-bc61-4150-99de-ebd8ce660504");
                    label = [ "let"; "="; "in" ];
                    mold =
                      {
                        out = Exp;
                        in_ = [ Pat; Exp ];
                        nibs =
                          ( { shape = Convex; sort = Exp },
                            { shape = Concave 40; sort = Exp } );
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
                                       "618d6969-feaa-4661-8554-f6269004d806");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "bb8c65e0-3728-4e8b-8630-a2915a2aaec4");
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
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "bf12c00f-1675-488c-a216-0e76c0e34a6f");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "aca57e6d-b8e0-4dd2-9f4c-3cfecd2a4416");
                                label = [ ":" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 24; sort = Pat },
                                        { shape = Concave 24; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "d5227312-0255-47e8-88ae-5bd239cd44f0");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "1a78cfb5-7dbb-4032-a9ce-5cbf10706ca7");
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
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "55557189-f7db-4743-ad92-3ea54d0063a9");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "ef5c0626-0d00-4078-955a-0fdfcb664dd0");
                                label = [ "->" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 13; sort = Typ },
                                        { shape = Concave 13; sort = Typ } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "0d2bdacf-f020-41f5-9966-ad908b7c47e0");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "c7890602-d5ec-4ec0-8a44-2668dd14195a");
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
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "9672a226-d9ec-4526-846e-0e246cc043ad");
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
                                 "aa682fe0-9047-4e4c-8f3d-d4b68769645c");
                          content = Whitespace " ";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "9affae1c-1ce3-4930-a6ac-640a2e63ba02");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "18fd614c-c1e6-4543-81a3-742399143372");
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
                    ] ) );
              ];
          };
        caret = Outer;
      };
    hidden_tests =
      {
        tests =
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
                                 "2cac70c7-5fef-45b6-a927-ddb752f76ab3");
                          label = [ "12" ];
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
                    ],
                    [] );
                ancestors =
                  [
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "1664558d-2e62-46c3-9c88-caa22ab1dc26");
                        label = [ "("; ")" ];
                        mold =
                          {
                            out = Exp;
                            in_ = [ Exp ];
                            nibs =
                              ( { shape = Concave 23; sort = Exp },
                                { shape = Convex; sort = Exp } );
                          };
                        shards = ([ 0 ], [ 1 ]);
                        children = ([], []);
                      },
                      ( [
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "40ee0555-bb7a-49a2-9251-3c0fa32b953f");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "642959b1-b1d6-4d58-aa79-4641d70a0eab");
                              label = [ "answer" ];
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
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "38e328b7-11d3-4bed-9f77-22f40648c18c");
                              label = [ "("; ")" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [ Exp ];
                                  nibs =
                                    ( { shape = Concave 23; sort = Exp },
                                      { shape = Convex; sort = Exp } );
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
                                               "9826a7d8-25e6-4c4d-820c-f7942a3f27bf");
                                        label = [ "13" ];
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
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "a9d0963a-7b29-459d-aa63-fce98144b240");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "2d317abf-a328-4891-b11b-af2d9eb395dc");
                              label = [ "==" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [];
                                  nibs =
                                    ( { shape = Concave 31; sort = Exp },
                                      { shape = Concave 31; sort = Exp } );
                                };
                              shards = [ 0 ];
                              children = [];
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "98a74450-fdba-4643-accb-6ec11d942274");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "7723c6a7-b31d-4a85-854f-f72361d02f5d");
                              label = [ "answer" ];
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
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "a3c42160-f777-47a4-b1df-6d9103418e8b");
                              label = [ "("; ")" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [ Exp ];
                                  nibs =
                                    ( { shape = Concave 23; sort = Exp },
                                      { shape = Convex; sort = Exp } );
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
                                               "b2678a5a-3a44-4c86-b078-4ecc12f2d8cf");
                                        label = [ "11" ];
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
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "33b11270-8eab-45f8-b228-a9cd2de34162");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "7f2ab39d-34dc-4316-8a6c-c02b4e49105e");
                              label = [ "+" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [];
                                  nibs =
                                    ( { shape = Concave 28; sort = Exp },
                                      { shape = Concave 28; sort = Exp } );
                                };
                              shards = [ 0 ];
                              children = [];
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "e64c3dbd-d3f3-486b-8778-2138b58f7030");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "15e158f1-3684-4b8a-8268-50fae11ba0a6");
                              label = [ "answer" ];
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
                        ],
                        [
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "84d9fab8-50f7-4c56-a0e3-b1ff991e2226");
                              content = Whitespace " ";
                            };
                        ] ) );
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "55cfff16-1d63-4d93-8b5b-6737090d3be5");
                        label = [ "test"; "end" ];
                        mold =
                          {
                            out = Exp;
                            in_ = [ Exp ];
                            nibs =
                              ( { shape = Convex; sort = Exp },
                                { shape = Convex; sort = Exp } );
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
                                     "5c63da68-4bb7-45d0-bfc7-3e32b873918c");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "4c2adb97-4977-4802-9449-8f22f62243f2");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "4bf5e622-922a-462e-9f4c-2df1c904e365");
                                        label = [ "answer" ];
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
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5f344169-a5b3-402d-bd15-4942b8be6be6");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 23;
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "737f2e2a-45e6-456f-b154-ccaf20d1c931");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e3851b6a-6ffd-4cdf-af4e-e2d81b3188a0");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "232d8939-6801-411b-8a3d-6e674ddcf4f8");
                                        label = [ "==" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 31;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 31;
                                                  sort = Exp;
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
                                               "0a0ffd7a-264b-4580-8e3d-6b714c3d5337");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "54f5a205-10c2-4d2c-9536-2ee76af001b9");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d5270590-ecbb-4d32-83df-09874f6dd9e0");
                                        content = Whitespace " ";
                                      };
                                  ];
                                ];
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "6448d8df-02a1-43a7-958f-4b5f9ab8001a");
                              label = [ ";" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [];
                                  nibs =
                                    ( { shape = Concave 38; sort = Exp },
                                      { shape = Concave 38; sort = Exp } );
                                };
                              shards = [ 0 ];
                              children = [];
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "7d0ffb22-2b71-4f0b-a866-a6b0a78e0821");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "a5db2e31-dfd9-4e7d-8e58-d5a06353166b");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f81a6f6c-3529-464a-90f4-d47aa5313efc");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "b6663469-21e0-4b00-a115-cdb7e6ed32f2");
                                        label = [ "answer" ];
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
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e75f31b6-68b4-4b0f-8b63-359cc61052b2");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 23;
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "428ace9d-bc7c-47d9-a11e-5b3ca5c0a8af");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "da8ef271-6ca6-4eae-a658-d443197630b8");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5db735c1-11eb-42bd-bc6f-ea1530b8251a");
                                        label = [ "==" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 31;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 31;
                                                  sort = Exp;
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
                                               "5a9daaec-a3ed-44f1-81c5-c756914a132c");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "3f9e3d2d-68b6-4ffe-8186-6199ed34c2ca");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "95034b87-67bf-45d0-bf4f-7b5a238c860c");
                                        content = Whitespace " ";
                                      };
                                  ];
                                ];
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "150390bf-0536-44de-9eb6-a32d4f2ed5dc");
                              label = [ ";" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [];
                                  nibs =
                                    ( { shape = Concave 38; sort = Exp },
                                      { shape = Concave 38; sort = Exp } );
                                };
                              shards = [ 0 ];
                              children = [];
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "e449df77-1d33-4118-b312-50cb886733cf");
                              content = Whitespace "\n";
                            };
                        ],
                        [
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "d1f7eff5-5a46-4d18-9994-6cc48f64b546");
                              label = [ ";" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [];
                                  nibs =
                                    ( { shape = Concave 38; sort = Exp },
                                      { shape = Concave 38; sort = Exp } );
                                };
                              shards = [ 0 ];
                              children = [];
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "478ef748-4435-44b9-b367-ffe9b5202b05");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "a97c22b2-b99f-4b26-b8d1-e72a9e017bc2");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e7df36ea-9aef-44b9-aaa7-8616f479c0ba");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0e457fc3-7300-4fee-8751-2291d47a2649");
                                        label = [ "answer" ];
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
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "20ac9fff-631e-4d3d-9c0a-0b77a88e0ded");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 23;
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "1f06c4fc-0464-408e-b4c9-2b10bc33a131");
                                                  label = [ "20" ];
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "c13ab220-75f8-49da-86b6-27da2f564bfb");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "36e3a145-e34b-4596-92ae-ca0a9fededc7");
                                        label = [ "==" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 31;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 31;
                                                  sort = Exp;
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
                                               "9a691466-f7c3-40ee-a207-4ede288effb7");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "33d0bf12-82d5-4ef8-9866-9329530fadb0");
                                        label = [ "answer" ];
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
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7df69671-75bf-44dc-9abc-c1bc04a255bc");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 23;
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "86a2caa6-ce01-4cb0-9591-eb3266ace0ea");
                                                  label = [ "18" ];
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ed6d78ab-37d5-4f4b-8518-93f137e7f835");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7081948d-de52-492a-8811-dbfbf17e05f7");
                                        label = [ "+" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 28;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 28;
                                                  sort = Exp;
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
                                               "407cf87a-6394-4392-8bc3-7f96b7d16a40");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "61a6f103-4979-4e0a-b0ca-991c86f18169");
                                        label = [ "answer" ];
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
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "3d4e1391-6a43-48e7-bb05-3e1558f3cfaa");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp ];
                                            nibs =
                                              ( {
                                                  shape = Concave 23;
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "9067363a-cef3-4470-a86e-9bfd7e5e7954");
                                                  label = [ "19" ];
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a9e16bab-388d-4d8f-b6a3-1c0f3d6e0f71");
                                        content = Whitespace " ";
                                      };
                                  ];
                                ];
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "d3ebb00f-b5d4-420f-a09c-0930f3f08068");
                              label = [ ";" ];
                              mold =
                                {
                                  out = Exp;
                                  in_ = [];
                                  nibs =
                                    ( { shape = Concave 38; sort = Exp },
                                      { shape = Concave 38; sort = Exp } );
                                };
                              shards = [ 0 ];
                              children = [];
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "eaae187d-d173-4b38-8503-cb9515638f00");
                              content = Whitespace "\n";
                            };
                          Grout
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "5a669da6-7676-495d-9c44-5ebd267377aa");
                              shape = Convex;
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "e125e807-f5f4-4cd4-b84f-2db46a230dad");
                              content = Whitespace "\n";
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "c9c72e8d-6685-4970-aa49-935b1e7df04b");
                              content = Whitespace "\n";
                            };
                        ] ) );
                  ];
              };
            caret = Outer;
          };
        hints = [];
      };
    wrapper = true;
    show_report = false;
  }
