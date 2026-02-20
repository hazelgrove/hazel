let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "a0000012-0012-0012-0012-000000000012");
    title = "List Literals";
    version = 12;
    module_name = "Tu_ListLiterals";
    prompt =
      "In Hazel, you can create a list by writing its elements between square \
       brackets, separated by commas. For example, `[1, 2, 3]` is a list of \
       three integers.\n\n\
       The type of a list is written as `[T]` where `T` is the type of the \
       elements. So `[1, 2, 3]` has type `[Int]`, and `[\"hello\", \"world\"]` \
       has type `[String]`. All elements in a list must have the same type.\n\n\
       Complete the let binding below so that `first_four` contains the first \
       four natural numbers: `[1, 2, 3, 4]`.";
    display_hint = "Fill in the list with the first four natural numbers";
    task_reference =
      "## Quick Reference\n\n\
       ### List Literal\n\
       ```hazelnoeval\n\
       [1, 2, 3]\n\
       ```\n\n\
       ### List Type\n\
       `[Int]`, `[String]`, `[Bool]`";
    your_impl =
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
                             "fff0f228-83b5-423a-a5bf-6288c08247d1");
                      shape = Convex;
                    };
                ],
                [] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "eb8e4d33-4075-41bf-a216-ddf14895a74b");
                    label = [ "["; "]" ];
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
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "c6a6760e-1b60-417d-bc39-4b51ed572bc5");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "83abc0c8-bfa1-44ad-8c05-ceac814ab17d");
                          content = Whitespace " ";
                        };
                    ] ) );
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "0f8d09b1-00b6-46fd-9d8d-ffdec1062de7");
                    label = [ "let"; "="; "in" ];
                    mold =
                      {
                        out = Exp;
                        in_ = [ Pat; Exp ];
                        nibs =
                          ( { shape = Convex; sort = Exp },
                            { shape = Concave 45; sort = Exp } );
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
                                       "e832b7a6-f5ce-42a8-8b1d-838bb7ceddb6");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "8c8c7200-1498-402a-a15e-d8da83002292");
                                label = [ "first_four" ];
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
                                       "cdd78d39-4c71-4188-9069-c349d229a7f4");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "5b63588f-c900-41b5-b1a0-15dccaf78321");
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
                                       "0f4a994a-81d9-4878-9e44-31ccc1b82dd4");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "d63850e5-cde6-4095-9148-3d53a18bd304");
                                label = [ "["; "]" ];
                                mold =
                                  {
                                    out = Typ;
                                    in_ = [ Typ ];
                                    nibs =
                                      ( { shape = Convex; sort = Typ },
                                        { shape = Convex; sort = Typ } );
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
                                                 "06b6c60e-44d3-4d18-94ff-14d9deaa4097");
                                          label = [ "Int" ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( { shape = Convex; sort = Typ },
                                                  { shape = Convex; sort = Typ }
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
                                       "606b9a04-a2bd-4ba8-8760-02eff12040de");
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
                                 "2d4a1405-6e7f-43a0-a875-66d81e296ff8");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "d1826f48-8ae5-4597-ac25-34363c745f80");
                          label = [ "first_four" ];
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
        refractors =
          {
            manuals = [];
            autos =
              {
                ids = Haz3lcore.Id.Map.empty;
                ephemerals = Haz3lcore.Id.Map.empty;
              };
            sample_cursor =
              {
                call_stack = [];
                index = -1;
                pinned_stack = None;
                indicated_call = None;
                time = None;
                seq = 0;
                step_range = None;
                pending_focus = None;
              };
          };
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
                                 "70774747-8a1b-4a18-8a04-c7e27af276de");
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
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "18795965-6a56-4364-861b-e14a9941f27b");
                          label = [ "," ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 44; sort = Exp },
                                  { shape = Concave 44; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "801ac55c-eb89-49e1-8b86-e49c59bc9366");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "e7ceee09-55cc-45b2-84ed-b5ec0ccc97f4");
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
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "54dd020c-ddcc-484f-aa85-b9656302817f");
                          label = [ "," ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 44; sort = Exp },
                                  { shape = Concave 44; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "766420c8-1f92-48c8-a3e0-ccf1443594d6");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "958a421b-dc61-4816-9595-cc469fec0619");
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
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "2bfb4b83-129b-4f13-a0a8-1319e5ad0dc0");
                          label = [ "," ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 44; sort = Exp },
                                  { shape = Concave 44; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "5c5c96b5-5ee0-4a32-bf57-5ddb5e903c44");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "4afb424f-3804-48c7-bbbf-5c8fb7338c64");
                          label = [ "4" ];
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
                               "49e75a54-76b4-4a09-93e1-d684f05295c6");
                        label = [ "["; "]" ];
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
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "0c9c7eed-17f7-436b-a757-c6d6b6f6e86f");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "50be6cc9-ec50-4a72-bd7b-1d95daa6930e");
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
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "7fd48f28-80ce-4f33-8551-2540c05641e5");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "4afbfb23-940e-49af-b090-f9db14fc4c4a");
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
                                     "4ee05e73-2b2c-4ce3-a03a-57784daa3586");
                              content = Whitespace " ";
                            };
                        ],
                        [
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "996faef1-26d3-4743-a11d-0afe0ef5608a");
                              content = Whitespace " ";
                            };
                        ] ) );
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "f3b4fda5-6797-4832-8e15-9e965da418e8");
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
                      ([], []) );
                  ];
              };
            caret = Outer;
            refractors =
              {
                manuals = [];
                autos =
                  {
                    ids = Haz3lcore.Id.Map.empty;
                    ephemerals = Haz3lcore.Id.Map.empty;
                  };
                sample_cursor =
                  {
                    call_stack = [];
                    index = -1;
                    pinned_stack = None;
                    indicated_call = None;
                    time = None;
                    seq = 0;
                    step_range = None;
                    pending_focus = None;
                  };
              };
          };
        hints = [ "Enter [1, 2, 3, 4]" ];
      };
    wrapper = true;
    show_report = true;
    setting_overrides = Tutorial.default_setting_overrides;
  }
