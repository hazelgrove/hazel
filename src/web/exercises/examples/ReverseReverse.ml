let exercise : TheoremExerciseSpec.t =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "f2132f9f-a452-481b-ba9a-c40e7d2346aa");
    title = "Reverse! Reverse!";
    prompt = "Show that this implementation of list reverse is its own inverse.";
    prelude =
      {
        refractors = Haz3lcore.ZipperBase.Refractor.init;
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
                             "f5b91c7f-fa01-4d57-a4cc-1921e725eb16");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "fba53d5d-d20b-4645-a272-bd8c02dbc023");
                      label = [ "fun"; "->" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 36; sort = Exp } );
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
                                       "ac194fc1-153a-4b53-a1e7-e86344c39ca9");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "e8103a71-2fe8-4158-be3e-78d39d25e17e");
                                label = [ "("; ")" ];
                                mold =
                                  {
                                    out = Pat;
                                    in_ = [ Pat ];
                                    nibs =
                                      ( { shape = Convex; sort = Pat },
                                        { shape = Convex; sort = Pat } );
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
                                                 "adc4ef2f-2df6-48b2-b412-d50089e1cc61");
                                          label = [ "t" ];
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
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "525e0ad5-f198-49dc-8b53-f4c48f1c6779");
                                          label = [ "," ];
                                          mold =
                                            {
                                              out = Pat;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 47;
                                                    sort = Pat;
                                                  },
                                                  {
                                                    shape = Concave 47;
                                                    sort = Pat;
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
                                                 "bf997339-6bb5-495f-b2b4-25d6707b0c8a");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "5ae69406-c974-43d7-bbc3-9524024a6c01");
                                          label = [ "h" ];
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
                                    ];
                                  ];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "d78f6422-522b-4de1-94fd-328445c8dd46");
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
                             "5660520c-2e72-4068-9742-4f887c94299f");
                      content = Whitespace "\n";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "7ce349a8-1a6b-4a4b-a1f6-82b2b045f2a7");
                      label = [ "case"; "end" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Rul ];
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
                                       "32395ec9-c926-4db6-b7ab-bb7e23ed17e7");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "d0f97a93-e4cf-4ebb-af94-8a2350ee9551");
                                label = [ "t" ];
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
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "64f4a28d-0afc-4406-a1d9-d936ef44b448");
                                content = Whitespace "\n";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "2247f4b4-17e9-4e22-8f4f-16ac60b3dce8");
                                label = [ "|"; "=>" ];
                                mold =
                                  {
                                    out = Rul;
                                    in_ = [ Pat ];
                                    nibs =
                                      ( { shape = Concave 43; sort = Exp },
                                        { shape = Concave 43; sort = Exp } );
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
                                                 "42661f47-04a9-47e9-9929-12f9cbe4a235");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "20e489e4-45c2-4239-a42e-d6b2a28398f5");
                                          label = [ "[]" ];
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
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "adf48369-6069-470e-ba7b-697dfe222c6b");
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
                                       "2c5cf30c-c53e-4698-ad88-2a45399c4296");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "b7211a5e-ea35-4ca9-a581-c1eec7bc380f");
                                label = [ "h" ];
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
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "9fa94d44-3167-4452-b497-2f903219d45a");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "bcbfa3c5-3c6b-403c-a904-077e42757d4c");
                                label = [ "::" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 29; sort = Exp },
                                        { shape = Concave 29; sort = Exp } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "a4657591-afe3-48cd-9dba-9c8171f84be6");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "e913334d-be9f-459c-8260-5fc18539e2c4");
                                label = [ "[]" ];
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
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "20a31035-b5e1-4691-9357-595d7ccc5317");
                                content = Whitespace "\n";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "13cd5cfe-7795-4a9a-b86a-4c0c95cb5fe8");
                                label = [ "|"; "=>" ];
                                mold =
                                  {
                                    out = Rul;
                                    in_ = [ Pat ];
                                    nibs =
                                      ( { shape = Concave 43; sort = Exp },
                                        { shape = Concave 43; sort = Exp } );
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
                                                 "f4de64e3-fc78-4fbd-aec4-acdc9e8ffec8");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "7b6de511-2647-4be1-b190-ccce93e443ed");
                                          label = [ "h'" ];
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
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "8185dba9-f6ce-4657-83d6-23e29917a91b");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "152ce841-cd4b-4555-bb90-ecea375473c2");
                                          label = [ "::" ];
                                          mold =
                                            {
                                              out = Pat;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 29;
                                                    sort = Pat;
                                                  },
                                                  {
                                                    shape = Concave 29;
                                                    sort = Pat;
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
                                                 "d1a0757d-6688-48da-bfae-4bd7e2560bf4");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "b7653629-5caa-4526-9589-b38c4d329740");
                                          label = [ "t" ];
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
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "e87c2495-2970-4680-82a9-7d8afe2450b0");
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
                                       "70f5623d-093a-4e52-9ba5-1f7bb37c7a58");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "518506e9-c579-4ac5-baa0-f18f820a19dd");
                                label = [ "h'" ];
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
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "b551db5b-c3ce-4adf-bbf7-1c182f70e3b4");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "801c8a63-e5cc-4923-b058-ac69bb35ee86");
                                label = [ "::" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [];
                                    nibs =
                                      ( { shape = Concave 29; sort = Exp },
                                        { shape = Concave 29; sort = Exp } );
                                  };
                                shards = [ 0 ];
                                children = [];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "5092d71b-f616-4e74-a7dd-1a8acf192230");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "2e6a2107-93ba-4b83-b5b3-e912575c57b9");
                                label = [ "snoc" ];
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
                                       "9d2253ac-92e9-469b-8459-108ad7eec52e");
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
                                                 "f0b662d0-8f95-46ac-a5fb-30e53ea3f7d9");
                                          label = [ "t" ];
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
                                                 "e3596916-8530-4b6f-ad21-91b86a2d7bb2");
                                          label = [ "," ];
                                          mold =
                                            {
                                              out = Exp;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 47;
                                                    sort = Exp;
                                                  },
                                                  {
                                                    shape = Concave 47;
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
                                                 "9b8a3923-f04d-4203-89ce-65896e3a9561");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "c2d6db2e-3ca9-48c6-afd2-42dda3440754");
                                          label = [ "h" ];
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
                                       "77f90754-a53a-44b5-9d7f-1fd8df5f501f");
                                content = Whitespace "\n";
                              };
                          ];
                        ];
                    };
                ],
                [
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "b7336a16-d79f-4536-9587-183927115667");
                      content = Whitespace "\n";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "f2130f6f-a452-481b-ba9a-c40e0d2346aa");
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
                                       "933b3eb9-1846-492f-931e-6b78c91fe18b");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "82fc27a4-5211-485c-9f5a-f3493c21fe34");
                                label = [ "snoc" ];
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
                                       "c80ccddc-c6cd-431a-9fe8-8c51b367f6f3");
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
                                 "6c0360a8-54c7-4dff-be34-9450c232f035");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "71d0f236-887a-440a-97d1-5cd88dd97763");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "b91fc811-20dc-47e8-95cd-d198b10c0afa");
                          label = [ "let"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Pat; Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 40; sort = Exp } );
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
                                           "2cbde278-e76c-4e5c-a55d-6b0e7754b1d6");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "5103b10f-6e2e-4902-867d-cddb63101e6d");
                                    label = [ "rev" ];
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
                                           "70ac2550-6f8e-41be-92c8-0c9e04a773c3");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "5a181f31-c50d-44eb-a6d0-1be4aace29e3");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "84e32484-31d8-40b2-8864-f3b2e786f437");
                                    label = [ "fun"; "->" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Pat ];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Concave 36; sort = Exp }
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
                                                     "fbb4c268-1da0-4fe5-a8db-d03af5a4a9c7");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "8b86e78a-382e-4d93-8413-f754772da2e7");
                                              label = [ "l" ];
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
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "efc8be12-f320-4b68-b1a9-110127748f34");
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
                                           "793b5840-a1dc-48e0-bb84-055d225be9ae");
                                    content = Whitespace "\n";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "f0683ed2-2afa-4e6d-8c5c-456a70ea5199");
                                    label = [ "case"; "end" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Rul ];
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
                                                     "07d7622a-175b-4c32-9b40-6651237d7dcc");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "a80fb760-d483-403f-a3b6-00f027db770f");
                                              label = [ "l" ];
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
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "2ecd2e56-3d10-433d-b4f7-ff8b87b69dcc");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "5bf71719-9dda-4ff0-99e2-91aefa1f95c8");
                                              label = [ "|"; "=>" ];
                                              mold =
                                                {
                                                  out = Rul;
                                                  in_ = [ Pat ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 43;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 43;
                                                        sort = Exp;
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
                                                               "46e07834-a8a0-45c1-a6c7-6f3691282e59");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "5727fb41-69c0-4114-91cd-2a3bf07c850b");
                                                        label = [ "[]" ];
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
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "2e5c1188-a3c9-4249-adac-51e759c23e4c");
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
                                                     "ca6d8c5c-1513-47b4-8a1f-1e4b6c8c48b9");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "f75deff5-04c0-45cd-94c3-01c873d30ab6");
                                              label = [ "[]" ];
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
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "a3c950c4-df63-4671-87cc-4c4685c19a41");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "eaa1912d-ebca-4076-bb87-4c107c59536c");
                                              label = [ "|"; "=>" ];
                                              mold =
                                                {
                                                  out = Rul;
                                                  in_ = [ Pat ];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 43;
                                                        sort = Exp;
                                                      },
                                                      {
                                                        shape = Concave 43;
                                                        sort = Exp;
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
                                                               "ff2260ff-0731-4f15-81e1-3ae1ae5c5deb");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "94e3d433-b782-4622-a91c-71d31f840973");
                                                        label = [ "h" ];
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
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "f7c1f210-1c0f-4fbe-8895-47bc0f66b843");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "5d147c6f-e405-4315-91da-d9a6747706bf");
                                                        label = [ "::" ];
                                                        mold =
                                                          {
                                                            out = Pat;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 29;
                                                                  sort = Pat;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 29;
                                                                  sort = Pat;
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
                                                               "f661de02-fecb-440a-b7bb-c9fde0754aa3");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "4d296daa-9e01-4d99-b7e0-38eeadf65a80");
                                                        label = [ "t" ];
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
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "6d8ebcf9-dfdf-4603-8684-64c464071f4d");
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
                                                     "bb805227-af20-4b35-98de-acd8050c2c3e");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "280f8b3a-4747-4688-b4bc-580599fc7081");
                                              label = [ "snoc" ];
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
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "a940af00-92be-4b50-a714-b5c1151ea9bf");
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
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "036df792-da8d-4fdc-ad3d-febed4f39cb3");
                                                        label = [ "rev" ];
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
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "a0251600-62e4-4588-97ba-939a9930df14");
                                                        label = [ "("; ")" ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [ Exp ];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 23;
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
                                                                  id =
                                                                    Option.get
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "21de271f-628d-4279-82e5-1f46430bf434");
                                                                  label =
                                                                    [ "t" ];
                                                                  mold =
                                                                    {
                                                                      out = Exp;
                                                                      in_ = [];
                                                                      nibs =
                                                                        ( {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
                                                                          },
                                                                          {
                                                                            shape =
                                                                              Convex;
                                                                            sort =
                                                                              Exp;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "fcf0e982-375c-4d30-a1ac-ed4085b95b8e");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Exp;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 47;
                                                                  sort = Exp;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 47;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "02bcbb14-c3ff-4cb3-9521-1d5808d0c4e8");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "82cdff11-bc48-424c-ab32-42223c72b8d2");
                                                        label = [ "h" ];
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
                                                     "e0124347-c435-4223-9662-0666459ba535");
                                              content = Whitespace "\n";
                                            };
                                        ];
                                      ];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ee6a6f1e-1f26-42ea-8cd5-e57ad0468deb");
                                    content = Whitespace "\n";
                                  };
                              ];
                            ];
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "7ba66ae0-5527-4b0d-8f28-2668e935f29a");
                          shape = Convex;
                        };
                    ] ) );
              ];
          };
        caret = Outer;
      };
    lemmas =
      {
        refractors = Haz3lcore.ZipperBase.Refractor.init;
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
                             "7cd1e7ce-f4c2-416a-af34-68c615a4ad17");
                      shape = Convex;
                    };
                ] );
            ancestors = [];
          };
        caret = Outer;
      };
    theorem =
      {
        refractors = Haz3lcore.ZipperBase.Refractor.init;
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
                             "f2130f1f-a452-481b-ba9a-c40e0d2346aa");
                      label = [ "theorem"; "="; "in" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat; Exp ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 40; sort = Exp } );
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
                                       "7996414e-651a-44cf-bb5c-3cdcff61a83b");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "8ccd632d-f59c-4d03-9a51-bd2cf277694f");
                                label = [ "rev_rev" ];
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
                                       "b49b54d0-76c7-420b-ac18-fe815d59f204");
                                content = Whitespace " ";
                              };
                          ];
                          [
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "e3373857-a28a-45ae-baae-6b8964925540");
                                content = Whitespace "\n";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "6b1bfd5c-651f-48a5-96c1-14d23c87253a");
                                label = [ "forall"; "->" ];
                                mold =
                                  {
                                    out = Exp;
                                    in_ = [ Pat ];
                                    nibs =
                                      ( { shape = Convex; sort = Exp },
                                        { shape = Concave 36; sort = Exp } );
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
                                                 "03abe80a-e471-4f77-8835-62b605d5f268");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "792593c8-9e05-460b-8dd1-0d516d7817ee");
                                          label = [ "xs" ];
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
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "d968bc44-4298-4f5c-b438-02882b22ddae");
                                          label = [ ":" ];
                                          mold =
                                            {
                                              out = Pat;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 24;
                                                    sort = Pat;
                                                  },
                                                  {
                                                    shape = Concave 24;
                                                    sort = Typ;
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
                                                 "1384c7a8-fa18-4c33-8913-e79848adc718");
                                          label = [ "["; "]" ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [ Typ ];
                                              nibs =
                                                ( { shape = Convex; sort = Typ },
                                                  { shape = Convex; sort = Typ }
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
                                                           "54b10331-262b-4976-8d07-5ea905c6e5ea");
                                                    label = [ "Int" ];
                                                    mold =
                                                      {
                                                        out = Typ;
                                                        in_ = [];
                                                        nibs =
                                                          ( {
                                                              shape = Convex;
                                                              sort = Typ;
                                                            },
                                                            {
                                                              shape = Convex;
                                                              sort = Typ;
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
                                                 "a9dc72cd-2b9a-437a-8a02-6c915fa16d34");
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
                                       "6f70555b-d5e0-428a-8963-aeae2b018745");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "d352380b-e6bc-468a-8181-12ab2219829a");
                                label = [ "rev" ];
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
                                       "2b7137a1-fe83-467b-b909-39c0bf423ec9");
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
                                                 "cb7424fb-8d71-4645-9ec6-ab4983b7992e");
                                          label = [ "rev" ];
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
                                                 "59901759-7e76-46b2-9b34-20a7eaa28f95");
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
                                                           "971c1de3-8e50-407d-bc25-2c16d4c57270");
                                                    label = [ "xs" ];
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
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "964bb63f-83f1-474f-9bcc-209e337adcaa");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "45de9fe6-a2df-4d95-9ff8-86d342d582af");
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
                                       "8c7354e2-e17a-44fc-b0cb-cf8da02451d0");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "5e256747-3b44-4bde-be10-87622b3e0968");
                                label = [ "xs" ];
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
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "25d1005b-2c1d-4d4f-bad1-b0facd6c586a");
                                content = Whitespace "\n";
                              };
                          ];
                        ];
                    };
                ],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "a36ade66-0230-4994-96f9-9daad38fe66b");
                      shape = Convex;
                    };
                ] );
            ancestors = [];
          };
        caret = Outer;
      };
  }
