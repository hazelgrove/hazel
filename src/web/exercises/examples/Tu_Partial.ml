let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "a0000008-0008-0008-0008-000000000008");
    title = "Partial Application";
    version = 8;
    module_name = "Tu_Partial";
    prompt =
      "Hazel does NOT support currying, but does support partial application \
       via explicit deferred function arguments using `_`\n\n\
       So writing:\n\
       ```hazelnostatics\n\
       f(1, _, 2, _)\n\
       ```\n\
       is the equivalent of\n\
       ```hazelnostatics\n\
       fun (x, y) -> f(1, x, 2, y)\n\
       ```\n\n\
       This allows you to create new functions from existing ones:\n\
       ```hazel\n\
       let keep_small = filter(_, fun x -> x < 4) in\n\
       keep_small([1, 2, 3, 4, 5])\n\
       ```\n\n\
       # Task\n\n\
       Using the provided `max` function and partial application implement a \
       function `non_negative` that takes a number `n` and:\n\
       - if n is negative returns 0\n\
       - Otherwise return n";
    display_hint = "The function should be the max of 0 and its other input";
    task_reference =
      "## Quick Reference\n\n\
       ### Multi-argument functions\n\
       ```hazel\n\
       let sum = fun (x, y) -> x + y in\n\
       sum(1, 2)\n\
       ```\n\n\
       ### Partial application\n\
       ```hazel\n\
       let double = map(_, fun x -> x * 2) in\n\
       double([1,2,3,4])\n\
       ```";
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
                             "4e08d394-f37b-41ce-83a1-31977372b930");
                      content = Whitespace " ";
                    };
                ],
                [
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "1ab7c798-5c46-4933-8e28-d10c49e81fdf");
                      shape = Convex;
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "72482f3b-5b6e-4f94-9bda-8b805e71d73d");
                      content = Whitespace " ";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "8ff0e2e9-1b85-455f-bb63-e93448f3a9fa");
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
                                       "6b33d915-7660-4487-a5d4-ef22c581da2a");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "8f9b1837-82c7-48f0-8806-a917dc00b589");
                                label = [ "non_negative" ];
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
                                       "382f51d7-5f86-4d4f-ac5d-9deaefb49a5f");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "39dc3ce6-fe57-4c4a-9e7e-e9ce06a5ceac");
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
                                       "ff9ba1e3-798f-4506-8106-a97a928098ae");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "e7d27f20-5ebb-4098-977f-7e8a8b8a8e52");
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
                                       "c7cdd089-ec92-4cf0-b661-3d79cdfe65bb");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "060e5faa-bded-45c3-b9ee-2a522fe52349");
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
                                       "7c44e973-8dbc-498a-9864-5bd35d442d95");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "c9f6fec8-6e16-46f0-a1f8-cc595730a2b3");
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
                                       "549a172b-8a59-47a7-b9d2-d321e37b41f4");
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
                                 "cd26668c-fb29-4a30-a3a8-925a56aceae7");
                          label = [ "let"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ Pat; Exp ];
                              nibs =
                                ( { shape = Convex; sort = Exp },
                                  { shape = Concave 45; sort = Exp } );
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
                                           "ec795aa4-cc6e-4f67-a56f-219e9e8fac1f");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "cfeea9a1-094d-4de4-9d4f-ace9e92c63d6");
                                    label = [ "max" ];
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
                                           "3c8333b1-b7e5-432d-ab93-770873928034");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c4bf1192-c9e9-4366-b9e8-2f3031a0c4e3");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "9ebd5ed5-d27a-462e-b603-99ee430a1ca4");
                                    label = [ "fun"; "->" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Pat ];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Concave 37; sort = Exp }
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
                                                     "20474c1b-2789-443e-b60b-c659accaca63");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "5cfdea82-1a63-46ad-8ca4-47b613893558");
                                              label = [ "("; ")" ];
                                              mold =
                                                {
                                                  out = Pat;
                                                  in_ = [ Pat ];
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
                                                               "6d2ef5d9-b81f-4c5a-ac43-29bcb7810d97");
                                                        label = [ "a" ];
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
                                                               "5756d8fe-e844-42fb-8569-846ae894da01");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "7f7bb2ea-128e-403b-805d-4dfccf984a1b");
                                                        label = [ ":" ];
                                                        mold =
                                                          {
                                                            out = Pat;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 24;
                                                                  sort = Pat;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 24;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "1f5b2d7b-d07a-4bd8-9358-1d6fa0252b2e");
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
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "6a9c39dd-1e17-4546-b2b7-45ee8788fe22");
                                                        label = [ "," ];
                                                        mold =
                                                          {
                                                            out = Pat;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 44;
                                                                  sort = Pat;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 44;
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
                                                               "22256000-56d0-427d-807c-6993e22acfab");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "c45cc4ee-3f62-4885-8cd8-01696ae08c8a");
                                                        label = [ "b" ];
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
                                                               "c30162ef-d7b2-4f96-adc3-88ebba7ff420");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "47e81e21-67d1-4206-a595-97f09b7c79f1");
                                                        label = [ ":" ];
                                                        mold =
                                                          {
                                                            out = Pat;
                                                            in_ = [];
                                                            nibs =
                                                              ( {
                                                                  shape =
                                                                    Concave 24;
                                                                  sort = Pat;
                                                                },
                                                                {
                                                                  shape =
                                                                    Concave 24;
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
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "331a48fa-9e2b-4440-a337-79022f506547");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "53fd042d-fea2-4901-8123-6d9eaee6ac2e");
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
                                                     "06788485-d012-40a1-81f8-371014e00145");
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
                                           "02997db9-dc2f-407d-a8f8-a600c8347f8d");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "40afa776-fa63-4a58-8172-03a0cb1b7dd8");
                                    label = [ "if"; "then"; "else" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [ Exp; Exp ];
                                        nibs =
                                          ( { shape = Convex; sort = Exp },
                                            { shape = Concave 36; sort = Exp }
                                          );
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
                                                     "e86ba877-526e-47e6-a6b7-1af2bf1386b9");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "43c5e1a7-4244-41eb-995f-32ebfb6e3202");
                                              label = [ "a" ];
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
                                                     "6fe3ea72-2ee6-41c6-bbc6-8387db1246f3");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "cda7cc1e-cda2-417b-a2ec-a98812501927");
                                              label = [ ">" ];
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
                                                     "36b5b3f3-3b1b-4cf7-a704-cf2f39577010");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "bd598eb6-b0ff-4eef-8833-2b1bbe4ac097");
                                              label = [ "b" ];
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
                                                     "8382b481-3e83-4571-8895-278739aee1fc");
                                              content = Whitespace " ";
                                            };
                                        ];
                                        [
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "a4e6b499-b833-48f5-bf25-f72b1021f84f");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "423d3637-1e8c-49a0-a4a7-fb87afe7d68a");
                                              label = [ "a" ];
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
                                                     "be6c5b10-bdb5-43e8-aca4-9cf432fd8759");
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
                                           "68649e63-bc76-40d4-bcd7-9b2f8167b22d");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "5274f6dd-aa32-4346-9a08-bf01033520a3");
                                    label = [ "b" ];
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
                                           "c227aa15-7d1b-42a6-b3e8-79a5a75e615a");
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
                                 "29b4a3f7-5461-415a-b604-a06c86c54bb8");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "18b208fe-6df0-4e40-8edd-23aac56e735e");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "c54ff096-575d-446c-b2aa-fae0934320b4");
                          label = [ "non_negative" ];
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
                                 "2e36e514-6878-47b6-8515-1e0e68981ef8");
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
                                           "67374a27-80d3-47ef-9ee8-e0ab38d2f20a");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c2a7c7d6-89f0-4fd9-8956-0876eaffd54d");
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
                                           "87c3e0e7-2049-4d1c-ad0e-6ba4c83c31f0");
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
                                                     "a4c1da81-692c-4720-97aa-47f7d8097515");
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
                                           "b348c6a4-21ab-4f6d-9c11-8551a30f7ed4");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "da5d8cba-fcca-43c1-903e-8e78841d9fcc");
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 31; sort = Exp },
                                            { shape = Concave 31; sort = Exp }
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
                                           "92ff921c-3c15-4e22-969a-fa060f3c64fc");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "cb561190-b05c-4a09-a8f9-f7a72a657f70");
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
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "99a78a0a-63c4-4e39-9d1b-c9c59236cf53");
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
                                 "cf6529d5-62ac-4ea8-b506-14018b797458");
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 35; sort = Exp },
                                  { shape = Concave 35; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "bf9b533c-1dea-4b4b-9ba2-3fd5e676e6a6");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "cd2d957e-dd51-48c1-9cfa-40e744734157");
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
                                           "86701e3a-c825-4433-92ae-0bfc32eb62ea");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "766eec02-3cbf-4980-bcf9-e6fb2261c3d7");
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
                                           "ec332c32-84cb-48fe-9d1e-f529efd7e6fa");
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
                                                     "2b5b1c3f-4587-47d0-815f-c1d7a96c8e92");
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
                                                        shape = Concave 25;
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
                                                     "dd194555-821f-4aed-af2c-b7fe0b919583");
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
                                           "099dca34-f083-4813-b56b-d270f1201c5c");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1f14310e-2e23-4ab4-959d-23a697b5f1e8");
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 31; sort = Exp },
                                            { shape = Concave 31; sort = Exp }
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
                                           "8685f085-10d7-453b-9884-18e698987ae5");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "91a44443-0b3e-40bf-a99a-9aeb26945abc");
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
                                           "0080e277-1d7b-46c1-9ed1-d16dd39a3305");
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
                                 "b57cd50f-3d16-40a4-974a-f288601c4c6b");
                          label = [ ";" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 35; sort = Exp },
                                  { shape = Concave 35; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "ff5c052c-c768-43da-ac1a-6f0d7e4e0de2");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "dc054ac8-714f-4de2-b176-6df62339ca2c");
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
                                           "b406bd5b-32a3-449a-bcf8-9c3bdfde1451");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "5b60d0a8-eeb3-4e6f-b29c-d57dd4522dd7");
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
                                           "31ad574a-63b8-4bdc-ba82-4309364327f3");
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
                                                     "aeeb26b0-23b3-4556-80d4-cd0852559ea6");
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
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "a3fe01a7-ab90-4997-bc55-b7a256cf6868");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "429ec61b-8c36-49b3-929d-a6ce6d89c81b");
                                    label = [ "==" ];
                                    mold =
                                      {
                                        out = Exp;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 31; sort = Exp },
                                            { shape = Concave 31; sort = Exp }
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
                                           "479ad893-440a-4c43-bc49-ad6a3dde5e98");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "f0402b2d-dff4-478f-b261-084d6d953441");
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
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "31e56fd4-49fb-447b-806e-37b30d2dbea1");
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
                                 "5b9d1838-8242-4b75-92a8-585c5513e17a");
                          content = Whitespace "\n";
                        };
                    ] );
                ancestors = [];
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
        hints =
          [
            "Keeps positive numbers";
            "Zeroes out negative numbers";
            "Works for more than 1";
          ];
      };
    wrapper = true;
    show_report = true;
    setting_overrides = Tutorial.default_setting_overrides;
  }
