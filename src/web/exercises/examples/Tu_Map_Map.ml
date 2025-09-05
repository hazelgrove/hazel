let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "f283b4c5-2439-0123-4abc-def167890223");
    title = "map map";
    version = 9;
    module_name = "Tu_Map_Map";
    prompt =
      "Now write a function that's like `map`, but operates on lists of lists. \
       Don't use recursion, use the existing `map` function.";
    display_hint = "";
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
                             "12f70008-5f58-46b8-9529-561aac317bcd");
                      content = Whitespace "\n";
                    };
                ],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "17ba24b7-d6ca-49eb-8a1e-89ebc6f4aaaf");
                      shape = Convex;
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "c83e8810-a5b2-455b-afb1-8f457d3e7ee3");
                      content = Whitespace "\n";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "4c8dbbb2-e7bf-450b-a928-855fc8ba2772");
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
                                       "264eabbc-856b-4a64-907a-f751e9274e0c");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "94023b81-ee7b-45a7-9684-c86766f4c50e");
                                label = [ "map_map" ];
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
                                       "26763070-c7a9-4e9f-b8ae-e3d111d5a68d");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "cf7d4df3-8ed5-459d-b32e-18a2a5bfc4b1");
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
                                       "4eff2916-ff12-45a4-b220-0cf7fc2b0511");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "949e30e3-e011-4f60-9e10-e02964c639a9");
                                label = [ "("; ")" ];
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
                                                 "53851920-1660-4f13-a0db-6095030ba971");
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
                                                           "a331d2b0-6eb8-4c1a-8a4d-8ecf2c875669");
                                                    label = [ "["; "]" ];
                                                    mold =
                                                      {
                                                        out = Typ;
                                                        in_ = [ Typ ];
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
                                                                     "49fc868f-d42d-4a46-894d-ad5ac67a48a4");
                                                              label = [ "a" ];
                                                              mold =
                                                                {
                                                                  out = Typ;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Typ;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Convex;
                                                                        sort =
                                                                          Typ;
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
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "64b1111e-82c6-44a2-83aa-3449e9b89f44");
                                          label = [ "," ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 47;
                                                    sort = Typ;
                                                  },
                                                  {
                                                    shape = Concave 47;
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
                                                 "14185227-bf83-47c3-b8a9-8e44c79eab3a");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "d87237cb-8ef5-4d46-a215-7406da99a020");
                                          label = [ "a" ];
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
                                                 "a85fe92a-8053-4766-94d6-f02ca5a4433f");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "95e407cd-1453-4dd8-b304-e76e66c5c416");
                                          label = [ "->" ];
                                          mold =
                                            {
                                              out = Typ;
                                              in_ = [];
                                              nibs =
                                                ( {
                                                    shape = Concave 13;
                                                    sort = Typ;
                                                  },
                                                  {
                                                    shape = Concave 13;
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
                                                 "024000d4-1e32-43f5-b846-b01488d62992");
                                          content = Whitespace " ";
                                        };
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "1608646e-8895-4180-8fb9-37e31ec3a42e");
                                          label = [ "b" ];
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
                                       "2f773c81-9e5b-445c-8af2-ba4befa69c00");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "1b9ce46e-e703-47d5-8931-a41ee5cb150b");
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
                                       "ced8e5b3-fbf1-48a3-ab1f-5d58f94f5796");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "2052261a-c78b-407a-8073-4f838216fd20");
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
                                                 "590661e0-6168-43b7-9500-f65cc6213923");
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
                                                           "6a9c3ab9-e6b6-48e4-9dd8-f838a05653ff");
                                                    label = [ "b" ];
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
                                    ];
                                  ];
                              };
                            Secondary
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "875d2bac-afab-4b23-81bc-2bdf21de1a8c");
                                content = Whitespace " ";
                              };
                          ];
                        ],
                        [] );
                  },
                  ( [
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "97def0e4-e464-4170-9907-161710b01fd5");
                          label = [ "type"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ TPat; Typ ];
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
                                           "5b5e2b05-893a-4fe6-b30e-559006c68c73");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e97d23f3-b603-4d34-8fa0-e08eaf183812");
                                    label = [ "a" ];
                                    mold =
                                      {
                                        out = TPat;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = TPat },
                                            { shape = Convex; sort = TPat } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "8d89b7ee-4e1b-4af2-a56b-eb2b8165d56c");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e8f8308c-24b4-46c6-9f7a-109aa061ae86");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "f030a17a-8707-4a29-bfa6-dcae785cd375");
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
                                           "f04fd6f7-ff5f-43f4-a858-3bc7d58b9cf9");
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
                                 "53378f4b-81c5-42b1-80b7-c1f47500c8de");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "f1f23492-c8b1-4ebb-ab1f-48222b859225");
                          label = [ "type"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ TPat; Typ ];
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
                                           "1acc75f1-1efa-4f83-98df-b12ac4ccc36a");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "a3678595-981c-4b02-a2ad-8ac04b6318b2");
                                    label = [ "b" ];
                                    mold =
                                      {
                                        out = TPat;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = TPat },
                                            { shape = Convex; sort = TPat } );
                                      };
                                    shards = [ 0 ];
                                    children = [];
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c9a5dc4d-86ad-4b09-8605-891c43838cc2");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "cb009c23-746c-4a9e-9d26-6eb01334206c");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "2e9e70ad-3d66-47ce-abb1-de1b63ee7aaf");
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
                                           "1958bf85-68e2-4b7f-843e-e9051a561c20");
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
                                 "0c29dde4-374b-4ddd-9cee-3ef5e5772a81");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "e605a84a-730b-40d1-8801-bec648939bda");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "d0effd13-70da-44f6-968c-93ee4cb51e0b");
                          label = [ "map_map" ];
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
                                 "548a4373-403a-4f09-b0ff-b369943ba8ee");
                          label = [ "["; "]" ];
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "6742d19f-bdaa-4fa9-bb27-e570d6f27595");
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "9dff0f20-528e-457c-943c-8edec8b92650");
                                    label = [ "," ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 47; sort = Exp },
                                            { shape = Concave 47; sort = Exp }
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
                                           "96f498e0-e8d9-4eb1-9ba6-75dc2862a74d");
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "ad8d954e-927e-4da0-bc48-568f7d5375b4");
                                    label = [ "," ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 47; sort = Exp },
                                            { shape = Concave 47; sort = Exp }
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
                                           "6aff0ee1-9210-45ff-aa72-b656d81ef733");
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
                              ];
                            ];
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "57ce0579-2e59-4555-8b08-40a71acc70c7");
                          label = [ "," ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 47; sort = Exp },
                                  { shape = Concave 47; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "788bc3f3-20f2-4f30-84c3-9f27f69b33bf");
                          label = [ "["; "]" ];
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
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "3198d2fd-1eab-4e20-9234-e1df90fabb8b");
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
                              ];
                            ];
                        };
                    ],
                    [] );
                ancestors =
                  [
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "5eb1f51a-b6e3-47df-910e-d3ee7aa3a191");
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
                                     "083f1598-f34a-42e5-8e21-47ec8a5e0a70");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "e7e32b20-7937-4695-8f39-48166b9c76f1");
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
                                     "edf73cbb-5acd-4515-8933-5b5b82cf26de");
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
                                               "ec262281-2e5d-4025-a7eb-8c627205bc5f");
                                        label = [ "["; "]" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp ];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
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
                                                         "663a5b81-70a1-4eb7-bdb0-78653e8282bd");
                                                  label = [ "["; "]" ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [ Exp ];
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
                                                                   "1fe220ce-0ad5-4ace-9a88-5b8683bb3337");
                                                            label = [ "1" ];
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
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "949c4468-5cb7-4252-a082-f0271ce71667");
                                                            label = [ "," ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          47;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          47;
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
                                                                   "8471dc77-a210-449d-994b-f9d94c08045f");
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
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "2bab83e8-0e58-452e-a1fd-6262ccfc3657");
                                                            label = [ "," ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          47;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          47;
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
                                                                   "fd0706a3-dde7-4d3f-aae0-6977d36de8dd");
                                                            label = [ "3" ];
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
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "0543466f-b1d2-485b-9efc-f344b29c47ae");
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
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "6b87ebc8-f3d4-457b-ba4b-3b1982538b7d");
                                                  label = [ "["; "]" ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [ Exp ];
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
                                                                   "ed268a31-7b9e-42a6-b4f6-6c09c719669d");
                                                            label = [ "1" ];
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
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7f00e9ba-f54c-4e3f-b444-d681312bd7b2");
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
                                               "7c061f2e-9ebc-461e-b07e-a4b6823bb516");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ff80590d-585c-45d2-892c-66501faf67a0");
                                        label = [ "fun"; "->" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Pat ];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
                                                {
                                                  shape = Concave 36;
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
                                                         "3415eb9c-41bb-4978-9901-65c735c24360");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "f26e3ee5-52a5-4378-a687-6daf1fd00f3b");
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
                                                         "ac0183b8-2328-4221-889f-6755d053df62");
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
                                               "b282d921-0b2a-43a0-bca6-7ce1f364ef07");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d0e2f57c-544e-4dfa-a2ea-2d1cbe36a39d");
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
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5bda28e7-80ab-4b49-becc-ac558157b87c");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5a5aae01-0a89-4391-8b42-88a77f34a34b");
                                        label = [ "<" ];
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
                                               "07987884-42d1-4989-9ee1-bac6632a7923");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "97508376-4bfa-41c6-a3b9-ece69d7fbdf9");
                                        label = [ "3" ];
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
                                               "d652bba4-cf39-4038-9377-4afd026b9c31");
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
                                     "1e3a64fe-9172-4fff-96d3-12df976e12d8");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "47ee7eb5-9b7e-4e72-982b-1e8ec56622bf");
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
                                     "2e821305-d8c1-41a0-9819-afe11e6000e0");
                              content = Whitespace " ";
                            };
                        ],
                        [
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "ce4b8a23-6751-491d-8c6e-db9e2197389e");
                              content = Whitespace " ";
                            };
                        ] ) );
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "c573882c-7524-4ecd-a6fc-d3d456dceaaf");
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
                                     "e369e8e6-5c0f-407f-b0dd-bd993eee1b81");
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
                                               "c68d52cd-885e-4cfa-8e54-dab94aca2f17");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "28ea7d50-f5e1-44d9-a8de-b05c654bcd9f");
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
                                               "7ad19b4e-295b-4e34-9bc3-15f0172d3da0");
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
                                                         "9bd39835-0cc9-4153-acad-5802772736c9");
                                                  label = [ "["; "]" ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [ Exp ];
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
                                                                   "10493808-2abb-43ee-89e3-e6a46c8c4c5f");
                                                            label = [ "["; "]" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [ Exp ];
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
                                                            shards = [ 0; 1 ];
                                                            children =
                                                              [
                                                                [
                                                                  Tile
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "5e1c3c49-a8cd-4726-bb40-d1960d184321");
                                                                      label =
                                                                        [ "1" ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Exp;
                                                                          in_ =
                                                                            [];
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
                                                                              }
                                                                            );
                                                                        };
                                                                      shards =
                                                                        [ 0 ];
                                                                      children =
                                                                        [];
                                                                    };
                                                                  Tile
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "2b398c9c-e3cd-4c57-9c0e-b33638bec20d");
                                                                      label =
                                                                        [ "," ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Exp;
                                                                          in_ =
                                                                            [];
                                                                          nibs =
                                                                            ( {
                                                                                shape =
                                                                                Concave
                                                                                47;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                47;
                                                                                sort =
                                                                                Exp;
                                                                              }
                                                                            );
                                                                        };
                                                                      shards =
                                                                        [ 0 ];
                                                                      children =
                                                                        [];
                                                                    };
                                                                  Tile
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "c7e536a9-ea67-4bc0-9220-46353cb5867d");
                                                                      label =
                                                                        [ "2" ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Exp;
                                                                          in_ =
                                                                            [];
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
                                                                              }
                                                                            );
                                                                        };
                                                                      shards =
                                                                        [ 0 ];
                                                                      children =
                                                                        [];
                                                                    };
                                                                  Tile
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "4de6341c-1b75-42e7-be4f-d6c7588e5e00");
                                                                      label =
                                                                        [ "," ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Exp;
                                                                          in_ =
                                                                            [];
                                                                          nibs =
                                                                            ( {
                                                                                shape =
                                                                                Concave
                                                                                47;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                47;
                                                                                sort =
                                                                                Exp;
                                                                              }
                                                                            );
                                                                        };
                                                                      shards =
                                                                        [ 0 ];
                                                                      children =
                                                                        [];
                                                                    };
                                                                  Tile
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "7b209bb2-9e59-4f13-a796-e72ba2a80e0d");
                                                                      label =
                                                                        [ "3" ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Exp;
                                                                          in_ =
                                                                            [];
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
                                                                              }
                                                                            );
                                                                        };
                                                                      shards =
                                                                        [ 0 ];
                                                                      children =
                                                                        [];
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
                                                                   "c76b30ba-89e7-4f2e-87cf-434daf4d1ae1");
                                                            label = [ "," ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          47;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          47;
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
                                                                   "c555ad3f-2966-40aa-a9ff-7fd6d8f6720c");
                                                            label = [ "["; "]" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [ Exp ];
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
                                                            shards = [ 0; 1 ];
                                                            children =
                                                              [
                                                                [
                                                                  Tile
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "d50ee694-0af4-430e-9142-c7f4c0a04b3c");
                                                                      label =
                                                                        [ "5" ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Exp;
                                                                          in_ =
                                                                            [];
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
                                                                              }
                                                                            );
                                                                        };
                                                                      shards =
                                                                        [ 0 ];
                                                                      children =
                                                                        [];
                                                                    };
                                                                  Tile
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "4855c3d3-87ee-4adb-a8dd-c505a4654087");
                                                                      label =
                                                                        [ "," ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Exp;
                                                                          in_ =
                                                                            [];
                                                                          nibs =
                                                                            ( {
                                                                                shape =
                                                                                Concave
                                                                                47;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                47;
                                                                                sort =
                                                                                Exp;
                                                                              }
                                                                            );
                                                                        };
                                                                      shards =
                                                                        [ 0 ];
                                                                      children =
                                                                        [];
                                                                    };
                                                                  Tile
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "fdbf440f-6c7b-43cb-b9f6-e88152121171");
                                                                      label =
                                                                        [ "2" ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Exp;
                                                                          in_ =
                                                                            [];
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
                                                                              }
                                                                            );
                                                                        };
                                                                      shards =
                                                                        [ 0 ];
                                                                      children =
                                                                        [];
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
                                                                   "13dd377d-a2ea-4ceb-abb8-6fa6c90f724a");
                                                            label = [ "," ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          47;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          47;
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
                                                                   "1cda1947-87fb-409f-b261-c4ff314a602c");
                                                            label = [ "[]" ];
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
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "4684504c-a428-48e8-9ed7-b66519f00723");
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
                                                         "2c7723dc-032c-4892-a12b-f1d16bc62e6b");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "09fa6611-9def-4438-95aa-f973bfb98076");
                                                  label = [ "fun"; "->" ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [ Pat ];
                                                      nibs =
                                                        ( {
                                                            shape = Convex;
                                                            sort = Exp;
                                                          },
                                                          {
                                                            shape = Concave 36;
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
                                                                   "0bc1c740-7ddd-45b0-af5e-135bc74f1a19");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "fb598843-5a33-4f4f-a63e-5c9e1dea89d4");
                                                            label = [ "x" ];
                                                            mold =
                                                              {
                                                                out = Pat;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Convex;
                                                                      sort = Pat;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Convex;
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
                                                                   "8cb95517-d58e-4047-a198-05e1f7713a4a");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                      ];
                                                    ];
                                                };
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "7495535d-bdac-40cc-9752-e0c2fb8ed7db");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "520e5c6b-254b-4f72-a07a-832550be599e");
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
                                                         "ba5ae5da-401e-45c2-bad8-20b9c40a4dd3");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "8be48164-52a6-476f-a225-71dbe809d55a");
                                                  label = [ "<" ];
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
                                                         "dde212c5-e4a6-4bb6-b84f-1b163433299e");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "58c663ff-b249-4b96-b6bd-5260f0e07aad");
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
                                              Secondary
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "8ed2e619-4fe3-414c-97b0-cedafdc71061");
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
                                               "54aff44d-d99d-4bc6-b6a9-c4961086d2fa");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "96bc1e80-b549-49cc-9a68-979c137b1be9");
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
                                               "7b2e19a4-143c-4e9a-9cd4-e5c2c5adaf62");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "9c6be009-0e7c-45e5-b742-77f8a56fd304");
                                        label = [ "["; "]" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [ Exp ];
                                            nibs =
                                              ( { shape = Convex; sort = Exp },
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
                                                         "d76dff15-0834-4783-a315-24dcec2e58b2");
                                                  label = [ "["; "]" ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [ Exp ];
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
                                                                   "8af1e085-077d-43aa-89d9-852fa90c1792");
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
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "d595168f-5435-48db-8e93-3d7c4476eac7");
                                                            label = [ "," ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          47;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          47;
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
                                                                   "72d76ad8-218f-4626-b951-5b2985401a71");
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
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "14417ac8-1f96-4297-9a5f-028828560737");
                                                            label = [ "," ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          47;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          47;
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
                                                                   "0b717301-b9ab-4929-99eb-32757266deac");
                                                            label = [ "false" ];
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
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "e2a19700-ffc1-4ca2-b643-92c1f6d43034");
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
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "4ef44d14-cb81-4a3f-a22e-fba59eba6cd7");
                                                  label = [ "["; "]" ];
                                                  mold =
                                                    {
                                                      out = Exp;
                                                      in_ = [ Exp ];
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
                                                                   "f827996d-1ba0-4fda-b971-c62fec82564a");
                                                            label = [ "false" ];
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
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "f4f46e59-3fef-42ac-99e2-399442c13d5e");
                                                            label = [ "," ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          47;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          47;
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
                                                                   "f41f8bbe-9bbf-46ca-8c23-d379f076b9d3");
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
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "a111bbd0-7cca-422f-bc0b-33aec9f03e9b");
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
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "240aaa8e-99b3-4825-b833-c88919749ed2");
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
                                            ];
                                          ];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "2bc82e5b-8571-4f5e-834c-76f110491d43");
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
                                     "1fcea25d-8af4-4936-a4fc-a545139b0d31");
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
                                     "5a19bee4-6fe6-4add-8505-60dcb033bdd4");
                              content = Whitespace "\n";
                            };
                        ],
                        [
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
    show_report = true;
  }
