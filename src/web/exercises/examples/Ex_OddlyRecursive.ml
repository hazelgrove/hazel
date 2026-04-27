let exercise : Exercise.t =
  Code
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "3335e34d-d211-4332-91e2-815e9e183885");
      title = "Oddly Recursive";
      module_name = "Ex_OddlyRecursive";
      prompt =
        "Write a recursive function that determines whether the given integer \
         is odd. \n\n\
         `odd(n)` is equivalent to `true` iff `n` is odd.";
      point_distribution =
        { test_validation = 1; mutation_testing = 1; impl_grading = 2 };
      prelude =
        {
          selection =
            {
              focus = Left;
              content = [];
              mode = Normal;
              anchor_caret = Outer;
              smart_rounded = false;
            };
          relatives =
            {
              siblings =
                ( [
                    Secondary
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "0b0ec27f-c146-402a-a66b-22801c008adb");
                        content = Whitespace "\n";
                      };
                    Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "e006bc60-1785-4994-932e-1ca708c23492");
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
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "9305b8d9-0987-4e0d-a3b7-022ee4832850");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "5cbec63e-8106-4089-bc76-41f059df550b");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "75bc5ad4-b6e1-44fd-88d1-c0e3ad9e2a2e");
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
                               "d3299002-785a-4255-9b56-5adcb1ceda72");
                        content = Whitespace "\n";
                      };
                    Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "cabc45ee-988d-41af-afd2-14094e331b66");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "2cfd2837-0b43-4857-b3d7-d644447310ba");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "82b8f1fc-3512-45b2-adbd-a3ac41bce582");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "c06672bf-b1df-4099-a2b4-5872977e889c");
                                  content = Whitespace " ";
                                };
                            ];
                            [
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "66b512a8-04fb-4d36-84f1-81f68be39cb4");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "e683b25e-c333-487b-9cd5-ce3b029191f9");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "12419bb8-71ec-4f43-9659-c3f83c6abca4");
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
                               "ef6d3f20-cafe-45a3-b159-40d0bc63f65b");
                        content = Whitespace " ";
                      };
                    Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "dd408785-4b74-49b5-99f2-51d3f9cb833c");
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
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "36466129-f187-46fa-94c3-f3c73ae9e09a");
                        content = Whitespace " ";
                      };
                    Secondary
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "7fa4cc6d-b065-40e8-8339-7ad2ae0acbbf");
                        content = Whitespace "\n";
                      };
                  ],
                  [] );
              ancestors =
                [
                  ( Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "d0c6d98c-a33e-4ed1-872c-be84560cd1bc");
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
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ed31b06f-2922-4361-b473-254978c27d89");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "b595d9bc-1b17-4b60-b44d-c59b00178d00");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "b294a14c-32aa-41a8-a223-48916924d314");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c0e4d10a-e193-4533-b881-6c18d173d639");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "3a0edb29-7101-4a05-a397-e7c6666757e5");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "711f7d0d-70db-4202-9009-aaca6c66a972");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "cffb0fed-3357-4cde-b94a-870a2445b621");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "8e2323ec-bebf-4cf1-a26f-78f245e844f8");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ae8d9296-2844-4038-85dc-2f348bbeac8f");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "187b4755-96e8-4aa4-9e1a-318c15396a51");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "33761a6f-2bb9-4204-a752-64034696ffab");
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
                                   "3bb3dae4-1d7a-4887-9515-979176db2fb4");
                            content = Whitespace " ";
                          };
                        Grout
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "88ec4496-28eb-4494-9bb4-a98b0fd8f4f3");
                            shape = Convex;
                          };
                      ] ) );
                ];
            };
          caret = Outer;
          refractors =
            {
              manuals = [];
              multis =
                {
                  ids = Haz3lcore.Id.Map.empty;
                  suppressed = Haz3lcore.Id.Map.empty;
                  ephemerals = Haz3lcore.Id.Map.empty;
                };
              sample_focus =
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
              autoprobe_target = None;
              pending_probe_cursor = None;
            };
        };
      correct_impl =
        {
          selection =
            {
              focus = Left;
              content = [];
              mode = Normal;
              anchor_caret = Outer;
              smart_rounded = false;
            };
          relatives =
            {
              siblings =
                ( [
                    Secondary
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "25f61bfd-cab7-4067-8b32-eacfb7c81c39");
                        content = Whitespace "\n";
                      };
                    Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "ecb01f9d-338d-454f-9259-41f2061fe6ba");
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
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "1cdce2fe-9ba9-44f1-b41d-f415763eae12");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "acdb8b00-bc08-4eb2-9f61-663d2a1ef9eb");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "04be7dce-42b3-4064-a7a2-cbd3895b2970");
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
                               "da8059a6-428d-4924-a49a-3c5d6ef88bb3");
                        content = Whitespace "\n";
                      };
                    Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "ac600f11-d4f2-4d20-9580-490d00e9b512");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "50c2a2df-df60-42cb-9ebd-1b71589e6940");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "552bf9b5-df56-4af6-8021-025740a221df");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "5b341ad0-f725-475d-8d4a-d9c4e32a5f6a");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "fae9fca2-ad70-43c4-83fa-1fd6562e189e");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "6022ef78-265c-472d-924e-0758484f8b4c");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a7962d39-18e2-4cfc-83b0-738d47b07234");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "4cb25a55-b618-4209-af98-c00a014451c4");
                                  content = Whitespace " ";
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "d43d1415-5415-4963-bc4f-c3692408b62a");
                                  content = Whitespace "\n";
                                };
                            ];
                            [
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "69af4740-54ce-4bf7-bfe7-bd8a75bf0f1f");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "bb039b3a-b935-4947-a3c2-30b7a1ecf08f");
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
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "232eb4f2-3794-4f8e-93c2-59ae4cd8c7ab");
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
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "b319a992-2964-4ef8-90b2-1bc518dd5994");
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
                                                      shape = Concave 2;
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
                                                   "547a55ef-4c34-457a-8f80-b2ff931b05cb");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "87525dee-f8c3-479f-a9a9-025660643b42");
                                  content = Whitespace " ";
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "57242d3e-0bde-4caf-9590-28bb634e95d7");
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
                               "dc5d9039-7a52-497f-b9fd-e8026d2c9f57");
                        content = Whitespace " ";
                      };
                    Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "13da937e-699f-4381-8ba0-128162e3095f");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "60526574-6deb-4ca6-a7a7-1a9193e3f651");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "cf78a8d1-feee-47d0-ab0e-47cf7c83e128");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "6f2e8c9f-02f5-4a66-aa12-ebdc135d7bbf");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "05efe84d-9a06-40e3-8385-125f1be1d8aa");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "3c6f7842-45b9-4c2d-b8ac-2c4554a88ff4");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "189c49db-0829-423a-be74-8e75179911f7");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "c5526fb0-26de-4374-9dc3-b889d167027d");
                                  content = Whitespace " ";
                                };
                            ];
                            [
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "05408d9d-986f-4531-a106-2a9b44796c4f");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "e66de40d-bad0-4be4-a5d5-6749ee5d4c47");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "2a058d05-ed95-4ae7-9a81-dd0da3372537");
                                  content = Whitespace " ";
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "8742f596-2ed0-417d-89f8-11ca54ceae11");
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
                               "b41f946a-4e18-4054-9f1e-b512cbf6aba7");
                        content = Whitespace " ";
                      };
                    Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "a0f83469-61c4-4f9b-9942-dc9dccc0cdbe");
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
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "223cab58-ae42-4084-9a3f-f0f39b9faf46");
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
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "01850913-5c8b-412c-af91-344b38c57456");
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
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "fdddefcb-5dc9-4451-973f-fef177ae6b5b");
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
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "e9c82c3d-e43f-4b9c-80cd-417bdf885df6");
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
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "e39a5f76-5e87-4702-9e97-494d99e4c2b7");
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
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "0effecef-993a-48ad-8acf-ebad8528a36b");
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
                            ];
                          ];
                      };
                    Secondary
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "269aa393-24bb-4f45-9736-a7ba4036fc7d");
                        content = Whitespace " ";
                      };
                    Secondary
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "38303003-3087-4660-9d7e-37f3967b11d0");
                        content = Whitespace "\n";
                      };
                  ],
                  [] );
              ancestors =
                [
                  ( Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "e7da6854-ef4c-4ee9-b588-a486496ae1f0");
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
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ab4d04bc-3598-4bb7-9ea6-13df3f99dceb");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ede61a5e-aca7-40cc-8530-fb47cffc8be6");
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
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "665e0d09-591f-4890-af71-44c016e1745b");
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e5b121c1-b775-4b68-ac3b-c5be52a6de9f");
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
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "b6dae494-feaa-466e-bb8e-60e3737f2380");
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
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "79cd9f8b-9df2-4aca-9683-5be08cd04bb0");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "9cee6990-d8c3-49d7-8be9-d51ad36ea441");
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
                                   "8da1116c-7c4a-4a3f-9119-b8ca4a08a1fe");
                            content = Whitespace " ";
                          };
                        Grout
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "4a17733f-afc1-453a-9341-b1698361062a");
                            shape = Convex;
                          };
                      ] ) );
                ];
            };
          caret = Outer;
          refractors =
            {
              manuals = [];
              multis =
                {
                  ids = Haz3lcore.Id.Map.empty;
                  suppressed = Haz3lcore.Id.Map.empty;
                  ephemerals = Haz3lcore.Id.Map.empty;
                };
              sample_focus =
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
              autoprobe_target = None;
              pending_probe_cursor = None;
            };
        };
      your_tests =
        {
          tests =
            {
              selection =
                {
                  focus = Right;
                  content = [];
                  mode = Normal;
                  anchor_caret = Outer;
                  smart_rounded = false;
                };
              relatives =
                {
                  siblings =
                    ( [
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "71ba4bbd-61e8-42fe-b9b0-a219ac196bb4");
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
                                             "78ed8643-d9b5-4b93-a55a-f90f148c98c8");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "35409e14-e422-426f-bbf7-0ac29a36dab0");
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
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "4d6d52cb-209e-4a16-b1ba-5c363e6df3f7");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "2f8ca917-3b82-49fd-8dd5-83139b0eb5f8");
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
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "5ed554ce-85cd-4783-b7b3-d3a9964e4d55");
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
                                   "9fe2fb75-0aaf-4e69-93b0-b1a07a113774");
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
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "4e4c377e-c1b1-480f-8284-f85b70b924cc");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "9b214a34-4806-4b22-92a5-18aa9d6269b4");
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
                                             "c7dbbcb5-ef77-4ca6-b19b-737a82aca852");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "56162939-b675-44d7-aabc-d8d9c419b7c7");
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
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "a8ee654d-0282-42c5-aa28-a8ac9491eff2");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "02cb8388-b674-4d05-b047-852275c897b9");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "82bfd022-49b8-4ec5-b801-98269b672285");
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
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "89e48239-a3ac-4678-b37b-ae5d43abf615");
                                                          label = [ "true" ];
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
                                          ];
                                        ];
                                    };
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "de94858e-7b43-4a07-8a36-c280ac7d2512");
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
                                   "452f3ff4-dc6f-49a6-8ce1-84d36ef69831");
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
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "fef7e530-7975-4ab2-8a53-ecf4adc1fb3e");
                            content = Whitespace " ";
                          };
                        Secondary
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "2cd52642-5379-496a-983a-0da5b3cf39f1");
                            content = Whitespace "\n";
                          };
                      ],
                      [
                        Grout
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "59acfba9-3226-4461-a482-2b2d9aadb700");
                            shape = Convex;
                          };
                      ] );
                  ancestors = [];
                };
              caret = Outer;
              refractors =
                {
                  manuals = [];
                  multis =
                    {
                      ids = Haz3lcore.Id.Map.empty;
                      suppressed = Haz3lcore.Id.Map.empty;
                      ephemerals = Haz3lcore.Id.Map.empty;
                    };
                  sample_focus =
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
                  autoprobe_target = None;
                  pending_probe_cursor = None;
                };
            };
          required = 6;
          provided = 2;
        };
      your_impl =
        {
          selection =
            {
              focus = Left;
              content = [];
              mode = Normal;
              anchor_caret = Outer;
              smart_rounded = false;
            };
          relatives =
            {
              siblings =
                ( [
                    Tile
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "5dfa4030-c0b8-47b9-ab1b-bc33a9b89e41");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "1fd69213-8e6b-4516-81a4-b681a5e36839");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "4e29da88-0d39-411d-b85a-23a2aedb07b7");
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
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "ea8ed903-f136-4c9d-8acc-6e5c0dbb153e");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "52671cee-bf99-48df-aba0-6518d6f06d99");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "609749f8-0e5e-4a04-afed-5479f4a58815");
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
                                         "6f50cae4-19db-4092-b0d8-54cfe609e32b");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "be46bf29-cb22-4789-8e7d-44f5dfaea749");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "c8be4f90-a5fe-4844-a2d9-b62cba9293ab");
                                  content = Whitespace " ";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "cd44256a-4196-48fa-b822-a0a76135622d");
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
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "d113895b-6715-4a49-9d40-4eab2246ed55");
                                  content = Whitespace " ";
                                };
                            ];
                            [
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "eaa082e4-9778-4ec3-9560-63ab3db7ae86");
                                  content = Whitespace "\n";
                                };
                              Tile
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "f1789fdf-64de-4c76-9be2-caa1989f85d2");
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
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "ef213741-1f01-4147-a64d-64f7669c7495");
                                            content = Whitespace " ";
                                          };
                                        Tile
                                          {
                                            id =
                                              Option.get
                                                (Haz3lcore.Id.of_string
                                                   "dbe3dcd8-b1f1-4f7e-b4f4-e1997826ed09");
                                            label = [ "n" ];
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
                                                   "80cc63a5-f549-4f38-b9ed-7759238b539c");
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
                                         "c7ae5e12-9aa7-411f-b098-f6c281ed54e5");
                                  content = Whitespace " ";
                                };
                              Grout
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "45681e47-a546-40d2-8f23-f00e3b242163");
                                  shape = Convex;
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "39e90854-8b82-4411-ae39-7a8100caa7c4");
                                  content = Whitespace " ";
                                };
                              Secondary
                                {
                                  id =
                                    Option.get
                                      (Haz3lcore.Id.of_string
                                         "a79c6e94-4b4c-4b38-b289-44fa4d7ddc6e");
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
                               "2fa240b4-22cb-4270-9983-88905d223555");
                        content = Whitespace " ";
                      };
                  ],
                  [
                    Grout
                      {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "c956a857-b3f2-47b9-8586-e36391ebaa2d");
                        shape = Convex;
                      };
                  ] );
              ancestors = [];
            };
          caret = Outer;
          refractors =
            {
              manuals = [];
              multis =
                {
                  ids = Haz3lcore.Id.Map.empty;
                  suppressed = Haz3lcore.Id.Map.empty;
                  ephemerals = Haz3lcore.Id.Map.empty;
                };
              sample_focus =
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
              autoprobe_target = None;
              pending_probe_cursor = None;
            };
        };
      hidden_bugs =
        [
          {
            impl =
              {
                selection =
                  {
                    focus = Left;
                    content = [];
                    mode = Normal;
                    anchor_caret = Outer;
                    smart_rounded = false;
                  };
                relatives =
                  {
                    siblings =
                      ( [
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "3ebaa0de-f35d-41e6-9a39-0733c9686ec4");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "cdd97cc6-9ebf-4757-8106-0e1a4eb4b3a0");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7e6541da-c6d2-46e6-bce7-1d673474c1eb");
                                        label = [ "odd" ];
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
                                               "3d144239-6cae-401d-b3e8-4551e054ea3d");
                                        label = [ ":" ];
                                        mold =
                                          {
                                            out = Pat;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 10;
                                                  sort = Pat;
                                                },
                                                {
                                                  shape = Concave 10;
                                                  sort = Typ;
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
                                               "0695db84-09b1-43b5-89a2-ae52f8489aa9");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a97e011c-a384-46cc-981f-50a225829dd8");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "50236197-3abc-4fbe-a430-a4c03cec75a8");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "98e0a108-1af8-4f7c-967d-6f78fe920534");
                                        label = [ "->" ];
                                        mold =
                                          {
                                            out = Typ;
                                            in_ = [];
                                            nibs =
                                              ( { shape = Concave 6; sort = Typ },
                                                {
                                                  shape = Concave 6;
                                                  sort = Typ;
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
                                               "15cfb38d-3e29-4828-9183-7a7bf36988c8");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "b70a45b8-2cde-4935-b65f-4d9adcdc3692");
                                        label = [ "Bool" ];
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0b755775-df38-434b-a761-bbaae92c3092");
                                        content = Whitespace " ";
                                      };
                                  ];
                                  [
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a5e5c602-f206-4095-985c-17dee5bef717");
                                        content = Whitespace "\n";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "74ca55c5-1229-48aa-b1f1-b3bd59fcfe71");
                                        label = [ "fun"; "->" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Pat ];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
                                                {
                                                  shape = Concave 14;
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
                                                      (Haz3lcore.Id.of_string
                                                         "fe26a817-dfc3-4989-90b6-56b9e9b3c2e9");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "0bdc40c8-ad05-4f1f-8620-a096759e9f51");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "fd6e0aec-1cfb-4a4c-a028-f25e238ac6e8");
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
                                               "00aaf4d8-5052-4e82-a9a8-7e29ffb98ec7");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "09f71ad4-d476-40ad-a21b-f70f36e4c25e");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ab9c9025-63ba-488e-9f8b-945ecc9f806e");
                                        content = Whitespace " ";
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "8eba6ca0-ee7d-4918-b13c-22c664f316bd");
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
                                     "973543dd-5333-41f4-bcb7-c9170f76c392");
                              content = Whitespace " ";
                            };
                        ],
                        [
                          Grout
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "8fa23aba-0296-4a8e-856a-b211d70fc134");
                              shape = Convex;
                            };
                        ] );
                    ancestors = [];
                  };
                caret = Outer;
                refractors =
                  {
                    manuals = [];
                    multis =
                      {
                        ids = Haz3lcore.Id.Map.empty;
                        suppressed = Haz3lcore.Id.Map.empty;
                        ephemerals = Haz3lcore.Id.Map.empty;
                      };
                    sample_focus =
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
                    autoprobe_target = None;
                    pending_probe_cursor = None;
                  };
              };
            hint = "always returns false";
          };
          {
            impl =
              {
                selection =
                  {
                    focus = Left;
                    content = [];
                    mode = Normal;
                    anchor_caret = Outer;
                    smart_rounded = false;
                  };
                relatives =
                  {
                    siblings =
                      ( [
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "ebc2c6d0-562b-4b57-8a6c-cdde94234cbe");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "b5757754-a281-4688-970f-ef5687ec6fa6");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "71383be6-f7f3-4dd0-8d92-f5130ee80971");
                                        label = [ "odd" ];
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
                                               "69c4f173-bdd7-4340-8e2a-5a88c08fc74c");
                                        label = [ ":" ];
                                        mold =
                                          {
                                            out = Pat;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 10;
                                                  sort = Pat;
                                                },
                                                {
                                                  shape = Concave 10;
                                                  sort = Typ;
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
                                               "00515328-9430-4ede-8599-ebd18cf8bd53");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "325c5114-322b-4751-9e8a-8ffcf1ceaf24");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "8d0a5ea2-63fe-49b5-903c-a949f317b1ac");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "438a6205-0d59-4ff9-b13b-72955a18ae65");
                                        label = [ "->" ];
                                        mold =
                                          {
                                            out = Typ;
                                            in_ = [];
                                            nibs =
                                              ( { shape = Concave 6; sort = Typ },
                                                {
                                                  shape = Concave 6;
                                                  sort = Typ;
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
                                               "99a639ac-b43f-42d4-8014-1117211b622b");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a04a7c2d-6493-4a14-a32b-2dc4f1524893");
                                        label = [ "Bool" ];
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a69b4d24-083b-4462-8d42-f3f13ef16867");
                                        content = Whitespace " ";
                                      };
                                  ];
                                  [
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "964bd2cf-947a-4b4a-94d3-f3fe5b14f984");
                                        content = Whitespace "\n";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "9f9d5d63-57b5-4fc7-ac21-077de50f0c47");
                                        label = [ "fun"; "->" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Pat ];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
                                                {
                                                  shape = Concave 14;
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
                                                      (Haz3lcore.Id.of_string
                                                         "b58080aa-67da-4242-ba73-b753513d84f1");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "b78081ac-8497-401a-83c9-630ae0feaab8");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "4da4d974-1d3c-45cc-b6d2-1fb783c89343");
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
                                               "7ee7aab7-cd26-480b-9e9e-8a1fdc7b5c10");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "78a09038-6c77-470f-a404-37ae7dc38e6a");
                                        label = [ "true" ];
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
                                               "21b4791b-bd12-4224-9a01-207d779599f0");
                                        content = Whitespace " ";
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "9ca73990-8fd2-43be-878e-a1fca5918932");
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
                                     "93e60cd3-5196-49ab-bc8b-5a95df7bf560");
                              content = Whitespace " ";
                            };
                        ],
                        [
                          Grout
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "a3b1da2a-eacb-47ce-b84b-09fd709c0074");
                              shape = Convex;
                            };
                        ] );
                    ancestors = [];
                  };
                caret = Outer;
                refractors =
                  {
                    manuals = [];
                    multis =
                      {
                        ids = Haz3lcore.Id.Map.empty;
                        suppressed = Haz3lcore.Id.Map.empty;
                        ephemerals = Haz3lcore.Id.Map.empty;
                      };
                    sample_focus =
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
                    autoprobe_target = None;
                    pending_probe_cursor = None;
                  };
              };
            hint = "always returns true";
          };
          {
            impl =
              {
                selection =
                  {
                    focus = Left;
                    content = [];
                    mode = Normal;
                    anchor_caret = Outer;
                    smart_rounded = false;
                  };
                relatives =
                  {
                    siblings =
                      ( [
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "f14c5ae3-4c40-476d-8fe2-204f542f3a14");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "56c9690e-9caf-4d99-8d8f-e5554947c12a");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "1ee02b3b-481b-4345-9aae-2f4f7b805e5a");
                                        label = [ "odd" ];
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
                                               "4c5141cf-1ed1-4220-a7d6-6d85709977ec");
                                        label = [ ":" ];
                                        mold =
                                          {
                                            out = Pat;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 10;
                                                  sort = Pat;
                                                },
                                                {
                                                  shape = Concave 10;
                                                  sort = Typ;
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
                                               "d431ac1e-8706-401e-8765-5a2b30b8d983");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a6a6a42a-729c-4374-882c-d7ed72eeb2b9");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "41ec5532-8890-49a4-b91b-dd88ebc80f92");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "4228e631-cc82-47ee-b718-720921f85c52");
                                        label = [ "->" ];
                                        mold =
                                          {
                                            out = Typ;
                                            in_ = [];
                                            nibs =
                                              ( { shape = Concave 6; sort = Typ },
                                                {
                                                  shape = Concave 6;
                                                  sort = Typ;
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
                                               "884c3e33-90db-4ed5-8ca2-0cc290b88bfa");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "8455c70b-6250-4a30-8e04-4caa46f4d541");
                                        label = [ "Bool" ];
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0bfa1448-ecac-49ae-9a0c-b399dc9c4503");
                                        content = Whitespace " ";
                                      };
                                  ];
                                  [
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d9842619-cdcf-45c0-800c-3663b43fb000");
                                        content = Whitespace "\n";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ceb87d6a-f1db-4e6c-af8d-d1679b56da2f");
                                        label = [ "fun"; "->" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Pat ];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
                                                {
                                                  shape = Concave 14;
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
                                                      (Haz3lcore.Id.of_string
                                                         "fe0ee65b-f464-4ee2-839b-3db4f6f0d5d5");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "65d15722-dbfc-46b9-a379-87991d9d6dc4");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "38354ad0-111a-4f58-ad95-90bcfc3e1343");
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
                                               "525fe3f0-67ef-46c1-8271-04bbea58afb1");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a871994c-cd85-4508-9429-d82582847480");
                                        label = [ "if"; "then"; "else" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp; Exp ];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
                                                {
                                                  shape = Concave 12;
                                                  sort = Exp;
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
                                                         "13284c56-0586-41cc-b2a8-abf8dca855b2");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "3510c7ad-e152-4b5a-8aa6-7791196eaa75");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "b43a5ca5-b474-4b49-91ff-e6ee660e271a");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "fd03f359-9e2b-47e9-90f7-61c2963bf9f5");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "48d58bdd-46d6-42d6-8c76-2a82b1c86377");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "2e720c9e-f1ef-493e-8758-e1e3abb722c9");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "a94467d9-9a1a-4da3-ae87-f7f936cbf228");
                                                  content = Whitespace " ";
                                                };
                                            ];
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "8acde6da-e720-4d32-9066-7a5ce0f0be30");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "9ccb0ffd-4a4f-4d8a-8c98-c996ec856c2e");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "7100d208-1ef6-41db-9f8e-52d82c676f54");
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
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "e53d8e0a-0044-444a-85b3-cd7a7adb1ba1");
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
                                                                        Concave
                                                                          2;
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
                                                                   "e067f7b1-9120-44bc-be2e-0576d7839e99");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "8f5ae58d-cdae-4b10-8bad-c8c659f4ffe1");
                                                  content = Whitespace " ";
                                                };
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "a26a25a2-b8ef-4c2e-810b-c87d5dc4a8fc");
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
                                               "6922b43b-5ab5-49ac-8cfa-835875a9f2f4");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "1df3dfb5-8c18-4ef8-bc1a-a061e3c3bda2");
                                        label = [ "if"; "then"; "else" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp; Exp ];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
                                                {
                                                  shape = Concave 12;
                                                  sort = Exp;
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
                                                         "783523f7-c8e3-41b2-bbe7-412d42015c56");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "aa7e567f-c84b-430c-b365-c14427237273");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "9fd6a977-6eab-45fd-9257-05d6ac96ecf5");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "9b3861a3-2de4-49ca-9311-587b38866427");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "e628c5c0-1b22-4415-8148-d708a8ed5d58");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "d7e147e7-c0f0-4075-9cec-e8db79222922");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "fafab22a-ab28-4989-b3e4-fe8db9dd261c");
                                                  content = Whitespace " ";
                                                };
                                            ];
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "5bda5b82-791b-4f34-912c-93cad207db65");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "89b10707-a467-4b38-bb1f-7b9e887b38a3");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "6ac07265-cddb-440a-93e0-433f5ef40cd6");
                                                  content = Whitespace " ";
                                                };
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "81d8445b-dbd2-4a29-a1b9-744a7b4218c1");
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
                                               "d7b3369f-c63f-4d0d-9ca6-53dec54b5ff2");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "76624388-7767-4512-92f2-eddc7a482834");
                                        label = [ "if"; "then"; "else" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp; Exp ];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
                                                {
                                                  shape = Concave 12;
                                                  sort = Exp;
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
                                                         "14fb6f8d-0cad-4920-a034-da55c509f135");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "e2c68c86-79f3-42f5-8922-6ba7a9663c7a");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "31c466a0-8f10-4526-ba69-4887ad2cf91f");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "77525872-ca1c-4391-867a-6a35c05f105a");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "e52d12f1-80ac-4626-9607-2c9109f566ca");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "53e49eef-fc09-48d5-9c6c-ca3a6d86e619");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "20ee4321-9e63-470d-b0d5-e2470d4301cb");
                                                  content = Whitespace " ";
                                                };
                                            ];
                                            [
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "d3c81be8-9216-4185-8ace-be81374ec85e");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "28642c57-084d-472b-b8f8-5ceefbcea907");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "663901f0-2b0e-4f27-8aaf-7a7d93f0998b");
                                                  content = Whitespace " ";
                                                };
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "2c1472e3-6943-4377-8458-266ac1f7515f");
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
                                               "aa575a7d-be16-4919-9af3-a99442664f5d");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ecc4246a-df63-499e-8d19-defcecc5e3a8");
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
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "fa8f88a6-87e4-487c-9095-f7a56ed0d039");
                                        label = [ "("; ")" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp ];
                                            nibs =
                                              ( { shape = Concave 1; sort = Exp },
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
                                                         "e50cf53e-3ae0-4ba7-9aab-a24f10e66386");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "a56d0a54-e7c0-4fc1-9408-0d4de84c1d39");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "dbe1c074-ee8d-477b-8e1d-5c5635d85f7e");
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
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "20beb62b-e4d8-455e-935d-9d152a790a7f");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "aa6fef37-716d-4ee3-9008-fd2de9b918ab");
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
                                               "7d83926b-65b8-4234-9e51-407d31651828");
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
                                     "5287e704-2dd8-4565-a6ae-fe2f31f91333");
                              content = Whitespace "\n";
                            };
                        ],
                        [
                          Grout
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "935d222c-d635-4d9a-94fb-8ca43e18fd8f");
                              shape = Convex;
                            };
                        ] );
                    ancestors = [];
                  };
                caret = Outer;
                refractors =
                  {
                    manuals = [];
                    multis =
                      {
                        ids = Haz3lcore.Id.Map.empty;
                        suppressed = Haz3lcore.Id.Map.empty;
                        ephemerals = Haz3lcore.Id.Map.empty;
                      };
                    sample_focus =
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
                    autoprobe_target = None;
                    pending_probe_cursor = None;
                  };
              };
            hint = "incorrect base case";
          };
        ];
      hidden_tests =
        {
          tests =
            {
              selection =
                {
                  focus = Left;
                  content = [];
                  mode = Normal;
                  anchor_caret = Outer;
                  smart_rounded = false;
                };
              relatives =
                {
                  siblings =
                    ( [
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "67fcecc4-0b58-42a4-b1fe-6c16dca11464");
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
                                             "b711fb0c-8629-441f-bd00-f456e322046f");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "898bf754-4394-44d9-8947-e2f4179b0265");
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
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "c40f95be-ebb1-45e4-8fa1-9849fe1cc9f6");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "baf80090-1211-4c78-a821-5daf95b09ff3");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "1cb0f572-10fa-48ef-b05c-584bc72e59f2");
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
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "1f35c2fa-d634-455f-ab10-c37adb439dbb");
                                                          label = [ "0" ];
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
                                          ];
                                        ];
                                    };
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "edd1e1f6-5c1c-43dc-8a0a-f680ea842ea3");
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
                                   "c73aa5ca-96de-4baa-90e7-dfd4d693d3c8");
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
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "16f674e5-a930-49d9-8d25-27e03d082b73");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "9f04f6f3-d8b8-43ba-9340-1ea479e0fa56");
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
                                             "56405288-4c67-4a1d-9c65-6c30c63707b2");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "3ab34efb-05b2-42b8-addf-5373c7eabd3e");
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
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "cd8906b5-a3ec-4be5-bb18-72685911e8bc");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "6a2725d7-0605-4db3-9cbe-1feea11c0946");
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
                                             "c5c34821-41c0-405d-99b7-fe0227cd1755");
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
                                   "c3137d42-97a9-4f35-b819-132ac8d4cf12");
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
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "a1392130-2249-4a92-bfc0-8bce805f9bed");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "3c712222-e82c-4483-97b1-7d0e076ed9ec");
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
                                             "4c4e5909-2434-4992-a498-4a2d55336d92");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "73bfe83d-ad38-4175-a141-f40c585ff483");
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
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "269a3573-8564-4d5b-92e4-49bad5784fbb");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "f5d3c94e-e867-43bf-8cf5-eda06143eeb1");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "212c75c5-eb48-48c8-baf0-599840affc9e");
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
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "61d76c87-cb3b-485c-bb04-eca9c1c60883");
                                                          label = [ "2" ];
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
                                          ];
                                        ];
                                    };
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "9e313fa4-b4d3-446c-acc1-0c857f58eda6");
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
                                   "aede9339-b4cf-4b95-b8f3-47b7e5115eb5");
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
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "8cb1c7a9-fad0-4d6d-ab1f-e5980c503ac9");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "6e7cf223-1522-4247-9424-6567164d7fde");
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
                                             "3124b639-60cc-4323-b30d-9a5e63c72dfc");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "e561e76a-6b07-4cdc-a3bf-a6a1c3b7cbbe");
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
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "9f4499e4-3893-4d35-943a-4b2a68ff817f");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "19b35ab8-a51a-4147-a34c-4f5bc7c92000");
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
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "5fa713c3-42fd-4eef-b855-b47b4d6574d1");
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
                                   "a56cf124-bb83-4642-8071-231d4124f7f9");
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
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "f0bcef9d-076b-43d4-af1a-abb592a75c9a");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "9ac3261d-2a37-4d62-8e31-54ba9c76e834");
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
                                             "da81150f-c2dc-47e3-9ffc-ae1f02f05341");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "3f39ec46-463b-4ac5-8e35-6b7e97ebc76e");
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
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "1a207e37-c857-4b05-b657-22fe986cf028");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "74968b85-5005-4da5-9d7b-de07392c6148");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "85534ecb-b1f1-4e3d-8ea9-3c9b74b3c322");
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
                                                          id =
                                                            Option.get
                                                              (Haz3lcore.Id
                                                               .of_string
                                                                 "f99f8720-4476-4450-b87b-3e56d5a60972");
                                                          label = [ "42" ];
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
                                          ];
                                        ];
                                    };
                                  Secondary
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "b9b9ba22-9193-495b-9614-7902f2a1a2ab");
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
                                   "eae8e827-5f57-48e5-bc48-9535c35116d2");
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
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "b0dc2540-e0d2-43b3-b357-594e23798e6a");
                            content = Whitespace " ";
                          };
                        Secondary
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "be04c0e5-d7b8-46ae-9e8d-f44a2cd89299");
                            content = Whitespace "\n";
                          };
                        Tile
                          {
                            id =
                              Option.get
                                (Haz3lcore.Id.of_string
                                   "319bd664-da72-4447-bb42-17e1d295f768");
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
                                             "08eb980f-4dd7-40c2-b49e-78a1c38b0ec1");
                                      content = Whitespace " ";
                                    };
                                  Tile
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "5613563b-7a29-4c65-8e16-d8ca7d237b75");
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
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "702c433f-65f9-43c6-a08a-ee54930b25de");
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
                                                id =
                                                  Option.get
                                                    (Haz3lcore.Id.of_string
                                                       "8fb69565-7b55-4f1d-a9d6-c45cd80b197a");
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
                                    {
                                      id =
                                        Option.get
                                          (Haz3lcore.Id.of_string
                                             "5c4e5378-b077-4c8c-83d7-034ece4e86f5");
                                      content = Whitespace " ";
                                    };
                                ];
                              ];
                          };
                      ],
                      [] );
                  ancestors = [];
                };
              caret = Outer;
              refractors =
                {
                  manuals = [];
                  multis =
                    {
                      ids = Haz3lcore.Id.Map.empty;
                      suppressed = Haz3lcore.Id.Map.empty;
                      ephemerals = Haz3lcore.Id.Map.empty;
                    };
                  sample_focus =
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
                  autoprobe_target = None;
                  pending_probe_cursor = None;
                };
            };
          hints = [ "zero" ];
        };
      syntax_tests = [ ("odd is recursive", IsRecursive "odd") ];
    }
