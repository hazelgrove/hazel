let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "5075659c-d40f-4970-9820-fccc91f38a3c");
    title = "Task 2: Gradebook Midterm Mean";
    version = 1;
    module_name = "TaGradebookMean";
    prompt =
      "Finish implementing the `midterm_mean` function. The function takes in \
       a gradebook and should return the mean of all the midterm scores as a \
       floating point number.\n\n\
       Feel free to define any helper functions you may find useful. The task \
       reference to the right provides functions and operations we think may \
       be helpful for this task.";
    display_hint =
      "Reminder the mean is the sum of a list of numbers divided by the size \
       of the list";
    task_reference =
      TaskRefDocs.compose
        [
          TaskRefDocs.column_projection;
          TaskRefDocs.partial_application;
          TaskRefDocs.fold_left;
          TaskRefDocs.length;
          TaskRefDocs.type_conversions_float_of_int;
          TaskRefDocs.function_definition;
          TaskRefDocs.binding_and_calling;
          TaskRefDocs.float_arithmetic;
        ];
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
                             "e110abf0-1dda-4b99-886d-a711d7175fa8");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "890342f0-9c2a-442d-8326-f240792712b5");
                      label = [ "fun"; "->" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [ Pat ];
                          nibs =
                            ( { shape = Convex; sort = Exp },
                              { shape = Concave 37; sort = Exp } );
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
                                       "50f9f2a6-5c42-4cc3-8320-aa3d00dff205");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "13f4acea-8318-43d0-9b20-8c8d40e1d85a");
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
                                                 "1c12ca35-e9e0-40c6-aeee-e8aea988d7c9");
                                          label = [ "gradebook" ];
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
                                       "a62c5bb1-68eb-41d1-9363-fb6f11012511");
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
                             "2bf51da2-073c-448f-b881-3f6b7378c55b");
                      content = Whitespace " ";
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "3c66edce-3bdf-4b62-b1f7-adf7c5123b12");
                      content = Whitespace "\n";
                    };
                  Grout
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "3565af42-ce05-4672-ba87-85061d3ab1be");
                      shape = Convex;
                    };
                ],
                [
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "63dd2608-1261-4a35-aa00-cedc6b33e8ea");
                      content = Whitespace "\n";
                    };
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "73b3eea8-9bdc-4967-8ae0-e460e6ed9760");
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
                                       "14601e4f-3d72-4c4d-a408-4012a0184969");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "01865224-f3d9-42e3-8d62-ffcf07e11577");
                                label = [ "midterm_mean" ];
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
                                       "b9222a36-2e97-4689-bc71-e2350fc062ae");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "7fbb4e4b-de65-42d3-a025-076482346fde");
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
                                       "e5e0b274-57f1-4725-a2d5-81d2e944c7f7");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "64f387c1-8541-4d6d-8782-e1855efcff8a");
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
                                                 "8cf3cada-f41e-4476-adfc-96a51202c879");
                                          label = [ "GradebookEntry" ];
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
                                       "c99082c3-980e-4f65-b6bb-2e7ad0756811");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "5c4545d3-302a-41a2-9973-f21ce14e963b");
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
                                       "7bfdb9dd-28b8-42c8-8bd3-7ae14f90d7a7");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "af103299-0cf6-4c08-9d0f-799a1c1b91f7");
                                label = [ "Float" ];
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
                                       "2cccb2da-5dfb-4078-8361-69bae49c3416");
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
                                 "93aa0b63-866a-49e1-947e-03e15cf34cc4");
                          label = [ "type"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ TPat; Typ ];
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
                                           "fd10cc49-32b8-4f16-9b26-a91935b7e198");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "afe57062-d86e-4bda-8027-c2512ab4584a");
                                    label = [ "Semester" ];
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
                                           "3957d86e-147e-4574-88f6-44d719c43a48");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "4df31d62-6cc4-421a-8d88-5db224a7b8ba");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "9ab2e225-912d-4ab2-b16e-a6f59f3e3033");
                                    label = [ "+" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Convex; sort = Typ },
                                            { shape = Concave 33; sort = Typ }
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
                                           "7425790c-5ae6-4c55-95fb-efa148ba93c2");
                                    label = [ "Spring" ];
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
                                           "a7472840-f5e5-46bd-a6cc-7cd338acdbba");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e4ed61fb-931d-4341-b076-cbe5bf21b080");
                                    label = [ "+" ];
                                    mold =
                                      {
                                        out = Typ;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 12; sort = Typ },
                                            { shape = Concave 12; sort = Typ }
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
                                           "f0dd4cfa-8ca5-4d8a-9489-18c10465b8cc");
                                    label = [ "Fall" ];
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
                                           "46b0aaed-f957-4bb8-b56c-5fcb3cc38d6b");
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
                                 "bfda255b-10cd-47fc-bbfd-3904fca39c97");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "a0e77a8a-afd3-4440-9d3c-6b394125ae0d");
                          label = [ "type"; "="; "in" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [ TPat; Typ ];
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
                                           "c915a4f4-77b2-46bf-9023-0e8e455fbb5d");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "162b4564-3afa-4a65-893c-0a066e98bdd5");
                                    label = [ "GradebookEntry" ];
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
                                           "5468c818-d266-4fb4-a4f8-5f861f04c680");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "e34f11af-d5d4-4b95-a5b8-9268b14c3bcb");
                                    content = Whitespace " ";
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "6368e7c2-937d-45be-a0bb-9db9b6e268de");
                                    content = Whitespace "\n";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "296f627c-a52c-49b7-ae47-53c826e01124");
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
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "cf22c571-33cd-41fd-8794-04d4b453e79a");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "61964c0b-a1cf-4470-ba9a-fabc2eb87392");
                                              label = [ "student_id" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "9e82a3b2-034b-4abc-9300-0195578b0a0f");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
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
                                                     "15824079-42c4-41c7-a424-dac052808668");
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
                                                  (Haz3lcore.Id.of_string
                                                     "2678732b-a033-4934-8433-cacf342269ee");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
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
                                                     "efdcb057-a0c1-4fed-b308-b3373cfeee3c");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "442de39a-6962-4917-82b6-065acb18d99c");
                                              label = [ "year" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "e951c494-81c1-448a-9876-7ecc77253327");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
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
                                                     "684ff981-1d2b-4fa2-9b21-d78f5cab3d5f");
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
                                                  (Haz3lcore.Id.of_string
                                                     "8434299f-7ca9-4278-816d-1c401bd37e9a");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
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
                                                     "1078df95-c91b-4eb2-8c6f-a7d3eccf5ca1");
                                              content = Whitespace " ";
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "53db3b0a-ce5f-48a1-a01f-06838d6006dc");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "af95ced0-572c-482e-ac0b-256f137c749c");
                                              label = [ "semester" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "12045570-bb9e-441b-8f38-a867fb8377f6");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
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
                                                     "b9eeebf7-4957-4c99-8f77-8c5606973220");
                                              label = [ "Semester" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "c236976b-9314-4589-82d7-4c017ab8ce3d");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
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
                                                     "84bb9e7c-36a7-42e4-8bed-f5fe133d7f23");
                                              content = Whitespace " ";
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "bf8c0504-79c1-44e9-a869-effbbaae1b68");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "b8d4141a-d630-4ad6-aa32-3ba04945f728");
                                              label = [ "quiz1" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "445c281c-da0e-4b3d-bf15-f176ac67b262");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
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
                                                     "b2e67478-4f6f-4e6c-b267-d03a22d0f6a5");
                                              label = [ "Float" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "1a6ed14e-b4f9-4e9f-b85a-8226347ebbfa");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
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
                                                     "a9cb7dc3-aa9e-42da-bd97-e29aa2d85566");
                                              content = Whitespace " ";
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "5b30cbb8-ed99-4983-b044-fb5f9bb7f836");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "f8651ab6-8859-4c43-8a2f-ddef336b71e9");
                                              label = [ "quiz2" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "325554b4-9c99-4aa0-8212-6b6e95881061");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
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
                                                     "db33ceb8-3b2d-4c74-aa0d-4473664f64c3");
                                              label = [ "Float" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "f9612a93-20af-4a8c-ad44-7a94afdde17f");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
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
                                                     "c8491da0-4258-4ffe-9bea-cf036f7b6c29");
                                              content = Whitespace " ";
                                            };
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "3f7b0e81-9c07-4132-a2d4-4afa1491e8d6");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "b0aa7e7e-addc-4233-9658-eec47d5c02e0");
                                              label = [ "midterm" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "b4f9a47b-506d-4a3a-9696-6abaeaf46e11");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
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
                                                     "a9749bea-8ff4-40f9-9ab6-ede17d47fc3b");
                                              label = [ "Float" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "e2a8ffc9-2b1d-47c5-bcb7-771da4d44cf7");
                                              label = [ "," ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 44;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 44;
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
                                                     "fae57e37-e934-430c-b994-3c49b37fabf7");
                                              content = Whitespace "\n";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "6b9dbb1f-612b-4a14-aa46-a95429d28215");
                                              label = [ "final" ];
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
                                                  (Haz3lcore.Id.of_string
                                                     "f8dc75d0-ab37-4088-8899-f9e070e4fecf");
                                              label = [ "=" ];
                                              mold =
                                                {
                                                  out = Typ;
                                                  in_ = [];
                                                  nibs =
                                                    ( {
                                                        shape = Concave 39;
                                                        sort = Typ;
                                                      },
                                                      {
                                                        shape = Concave 39;
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
                                                     "080fc923-c1e4-4b40-8122-c288d7b2da7c");
                                              label = [ "Float" ];
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
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "52ff36f0-76eb-4efa-8edc-866988f70d38");
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
                                           "d63f7c57-373e-454b-a1f0-b853324bb012");
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
                                 "d3bacf8e-6f9c-4b31-8106-f6dd3fd05663");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "ad325a3c-3312-4971-b81d-8c9e8286468d");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "09d1278f-b912-4c39-922e-83a874aa775f");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "b6c5e421-fd96-49b7-be3b-c74ce6ec8203");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "d30cf63d-08a1-4567-9ff7-ed6a89021724");
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
                                           "877b6082-13db-4895-8810-b3a1eae47308");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c201ddb3-6dc3-45ca-80ae-cf05b731a465");
                                    label = [ "gradebook" ];
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
                                           "93c90b2c-ec81-4eaf-95aa-efba0dd40492");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "152155bb-451d-4c98-b42c-02744bd0c37d");
                                    label = [ ":" ];
                                    mold =
                                      {
                                        out = Pat;
                                        in_ = [];
                                        nibs =
                                          ( { shape = Concave 24; sort = Pat },
                                            { shape = Concave 24; sort = Typ }
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
                                           "8f3c01ab-4b33-4d09-b26c-c4e743ed9db8");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "0547412b-012d-4411-b665-aabe8b66431a");
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
                                                     "31f6e3b6-760c-434d-ad73-961944072c22");
                                              label = [ "GradebookEntry" ];
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
                                           "45665523-0a47-48dc-964a-8e5a90ae21e0");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "fed7c068-481d-4afa-a696-1ca0eaabf170");
                                    content = Whitespace "\n";
                                  };
                                Projector
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "d1707045-55f7-494a-ade1-924b8b8afc39");
                                    kind = Table;
                                    syntax =
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "ed99b998-1c2f-4852-a2ad-5931164fad4f");
                                          label = [ "("; ")" ];
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
                                                           "f7b78504-91e0-4c2c-94f4-f30546fdafb1");
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
                                                                     "35b1cfbd-16cb-4ebf-bfde-361ef19d81eb");
                                                              label =
                                                                [ "("; ")" ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [ Exp ];
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
                                                                               "cbb22583-5973-4a12-8b8c-dd3a5f3d3e67");
                                                                        label =
                                                                          [
                                                                            "student_id";
                                                                          ];
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
                                                                               "d6f5ec4f-3782-442f-ab79-36224ad80961");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "35240632-87ee-4ed0-9857-2af9b3ab1fc8");
                                                                        label =
                                                                          [
                                                                            "1";
                                                                          ];
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
                                                                               "4338b012-742d-4c9e-b516-6d7f88a716e1");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "89999cb6-6b2e-4ab8-845b-08e0d02388c8");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "894d6bac-3f98-4356-b20c-33f4d199b33e");
                                                                        label =
                                                                          [
                                                                            "year";
                                                                          ];
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
                                                                               "fd673766-2f7b-47c5-b820-2d63f16a2347");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "ace49ecf-9cbd-4728-8845-6f18b433db2a");
                                                                        label =
                                                                          [
                                                                            "2025";
                                                                          ];
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
                                                                               "14f7df12-c797-455a-976d-b1055cca2cca");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "7ede2c3a-e467-4276-ba2b-2d6dda431bc2");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "49065819-864a-49e2-b686-337bfd820a2e");
                                                                        label =
                                                                          [
                                                                            "semester";
                                                                          ];
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
                                                                               "3f927e0b-1b2a-442e-b695-b7403616bcad");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "4b0e8c4d-8444-4f1f-a318-dac8f4e68f9a");
                                                                        label =
                                                                          [
                                                                            "Spring";
                                                                          ];
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
                                                                               "a2bb7071-2d37-4f97-bdde-ed4ae77f77cd");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "510ce938-565e-4074-83a6-b49e91730345");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "fc897369-eb7f-475e-ba3c-7df6c4935b58");
                                                                        label =
                                                                          [
                                                                            "quiz1";
                                                                          ];
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
                                                                               "e1c0cb85-4de7-4c3b-b350-fe99fe68b1b1");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "ed40554b-32b3-44d8-af7a-149de4597bdc");
                                                                        label =
                                                                          [
                                                                            "7.000000";
                                                                          ];
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
                                                                               "ba41514c-8ac9-43b5-9896-071f0ab774c0");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "378f8693-6357-49e8-ae7a-70f97c9f041b");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "836a2ff3-876b-40e5-bc1a-b1232070e218");
                                                                        label =
                                                                          [
                                                                            "quiz2";
                                                                          ];
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
                                                                               "4df270e8-0b66-43c8-9a77-4db165e5f467");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "88eff70b-175d-40b6-b1bd-5a103ccf1340");
                                                                        label =
                                                                          [
                                                                            "5.000000";
                                                                          ];
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
                                                                               "527a8509-477a-4d56-aded-dfe756d156d6");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "8161d32c-f31a-407c-aadb-79f4fa0b4020");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "a7cd49c4-0ccf-43e5-a8f1-bf947daa74ee");
                                                                        label =
                                                                          [
                                                                            "midterm";
                                                                          ];
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
                                                                               "b0c03234-ed86-496b-ab10-70ae39e0e84c");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "f3c98f8c-50bd-4354-8c2f-2846162db034");
                                                                        label =
                                                                          [
                                                                            "85.000000";
                                                                          ];
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
                                                                               "8360006e-2775-4748-97b7-f9ad7889e433");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "d09e73b5-26f8-43f9-9cf1-6228695f336e");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "6db201c6-9896-4a52-bc30-d53c4c9305e1");
                                                                        label =
                                                                          [
                                                                            "final";
                                                                          ];
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
                                                                               "a7ad830c-eed3-4888-afd3-01b2cc7d4c32");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "bfc5af79-2049-42d7-bff4-fb50cd668dc1");
                                                                        label =
                                                                          [
                                                                            "88.000000";
                                                                          ];
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
                                                                     "0c415df6-0a18-4443-a5b9-923658685ee1");
                                                              label = [ "," ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
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
                                                                     "23c8a7dd-e13a-488a-b6dc-06378d61b3f6");
                                                              content =
                                                                Whitespace "\n";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "4a208885-ebc1-4f2d-88ae-0cab5b8e4c86");
                                                              label =
                                                                [ "("; ")" ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [ Exp ];
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
                                                                               "783874b9-31a6-4177-8771-3e7189950d54");
                                                                        label =
                                                                          [
                                                                            "student_id";
                                                                          ];
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
                                                                               "5c117bf0-82dd-478b-9ac4-309ea822908b");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "e7248081-1678-44c1-9e8c-5aba320c174b");
                                                                        label =
                                                                          [
                                                                            "2";
                                                                          ];
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
                                                                               "9efce2b2-cf2c-40d1-9d3e-bc7fe134e95c");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "fe7c7ffe-b7c8-4fa4-9b13-692b00a2c9df");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "971d7cf0-72c0-40cd-98b3-70bbefe01aa9");
                                                                        label =
                                                                          [
                                                                            "year";
                                                                          ];
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
                                                                               "8c3bede1-64ea-44bd-b7ef-4b1011dc896b");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "b392cb5a-ebc9-41fe-b4e4-e21016b35dc4");
                                                                        label =
                                                                          [
                                                                            "2025";
                                                                          ];
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
                                                                               "5d49044d-b3e7-41ec-be69-757d4aef323c");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "6ed0590b-9c04-4e63-b0a2-1259616192ab");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "6bbbbbd3-d967-4b53-9002-d503826c68f9");
                                                                        label =
                                                                          [
                                                                            "semester";
                                                                          ];
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
                                                                               "3977516d-424a-4b7c-a9c8-65149608dede");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "3a80bd54-cd9d-4bea-8a04-d7b1fd6e3122");
                                                                        label =
                                                                          [
                                                                            "Fall";
                                                                          ];
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
                                                                               "f1469433-b10c-4ac5-83de-3ab100535b14");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "542b4190-8a4d-4e86-8180-ff61874172c8");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "1c639c4a-296c-47d3-85ad-b5c54b6d6d0c");
                                                                        label =
                                                                          [
                                                                            "quiz1";
                                                                          ];
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
                                                                               "e890ef26-304c-45cf-ab82-b51e5a075fcd");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "4851ef66-07f4-454e-aef3-ed08f3a7145d");
                                                                        label =
                                                                          [
                                                                            "5.000000";
                                                                          ];
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
                                                                               "99069876-e826-4b15-8933-8b456e858c7c");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "5bcab742-03d0-4d81-8b33-da2792d51dbf");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "01ea9833-c048-418c-9455-aa2f81f4f7c5");
                                                                        label =
                                                                          [
                                                                            "quiz2";
                                                                          ];
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
                                                                               "f83bbf7e-ef1c-4ec6-a2c6-0a391c5cb81a");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "ecee0a8e-48b1-4de4-a8d4-3ff24bfedf92");
                                                                        label =
                                                                          [
                                                                            "8.000000";
                                                                          ];
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
                                                                               "856ff8d7-f706-445e-bc42-ae9cefb660ed");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "e79e880d-cac3-4651-9ca7-bd04173cb21a");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "e3a5f919-966e-49d9-a147-4ea55313fc77");
                                                                        label =
                                                                          [
                                                                            "midterm";
                                                                          ];
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
                                                                               "77dda2b2-18d2-4f01-9d08-e8970388aa74");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "a90b7eeb-7cde-4e8b-ab9f-7115d3046c8f");
                                                                        label =
                                                                          [
                                                                            "90.000000";
                                                                          ];
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
                                                                               "73e62fd9-48c9-4aed-bad7-2e1949131aff");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "ee1da9b7-4f07-4774-99f3-6a74e709d374");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "cb14712b-8274-4e28-86f1-ba47bb8d6b97");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "640baf19-0637-4a87-a9f0-ec8197b04b4e");
                                                                        label =
                                                                          [
                                                                            "final";
                                                                          ];
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
                                                                               "d162d904-bd95-4ae0-9c3c-181f2d96ef87");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "04bc269a-8c03-4bc6-8899-7c9a95fc811b");
                                                                        label =
                                                                          [
                                                                            "82.000000";
                                                                          ];
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
                                                                     "3d710c5c-a2b1-45d4-a923-3ff8b2488e97");
                                                              label = [ "," ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
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
                                                                     "804d94dc-ead8-4ec0-87c1-fff4d41fbe63");
                                                              content =
                                                                Whitespace " ";
                                                            };
                                                          Secondary
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "3fbd626c-315b-4a37-8483-7a2d82cb500d");
                                                              content =
                                                                Whitespace "\n";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "f342c819-8c65-47a6-b0dc-90dc456b42ae");
                                                              label =
                                                                [ "("; ")" ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [ Exp ];
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
                                                                               "59e5afb5-82d0-4ba7-8205-54ec416a84bf");
                                                                        label =
                                                                          [
                                                                            "student_id";
                                                                          ];
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
                                                                               "a54afd22-d576-4874-9b69-11e68d26248b");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "caa90217-a25e-41d0-82e5-414f45275962");
                                                                        label =
                                                                          [
                                                                            "3";
                                                                          ];
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
                                                                               "e36835be-477d-4167-8ff0-aeee1794f6ef");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "6704e86e-f26f-4528-9f01-68c605bada91");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "96d58466-c6d9-4c99-886b-4268b73ff5b7");
                                                                        label =
                                                                          [
                                                                            "year";
                                                                          ];
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
                                                                               "b42a2c34-e025-463b-810b-ae0e412ffc97");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "405294a6-f47e-40db-8159-e40b0be64919");
                                                                        label =
                                                                          [
                                                                            "2024";
                                                                          ];
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
                                                                               "a3edac93-b07f-42c6-895a-b9331df1f8d9");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "a2a3f7ca-5ae2-4ada-91f9-baa654632797");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "8eac74e4-96fa-4848-980c-f148d30097fe");
                                                                        label =
                                                                          [
                                                                            "semester";
                                                                          ];
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
                                                                               "36af0ca3-aa45-40f3-bde5-6bfe971f7dbd");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "3fe4887e-d8bb-4e17-9afd-ff9de6211f77");
                                                                        label =
                                                                          [
                                                                            "Fall";
                                                                          ];
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
                                                                               "473e46c3-1de6-4d76-90e3-60f9e4214123");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "1a2ac3aa-2ab5-4c72-a8b8-445bff0e1cec");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "62fadad7-303d-4e4b-bf1a-e3102a741a3e");
                                                                        label =
                                                                          [
                                                                            "quiz1";
                                                                          ];
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
                                                                               "9898481e-26b8-4bf7-8f1f-2ed59d631bbe");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "d6092443-55e1-4163-ac89-8699cd060104");
                                                                        label =
                                                                          [
                                                                            "8.000000";
                                                                          ];
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
                                                                               "f8f9e96b-8e42-462b-a6b2-b21f7d8fb6a2");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "407d0ad0-9539-4160-a00f-d19d293d3ff6");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "d5a5cd05-9cd2-4797-bbd9-f77fde06f72c");
                                                                        label =
                                                                          [
                                                                            "quiz2";
                                                                          ];
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
                                                                               "37fac868-c8d0-422a-8e46-f08ebf3eaa9a");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "6ed9a063-22e5-498c-b685-35fc49e40831");
                                                                        label =
                                                                          [
                                                                            "7.000000";
                                                                          ];
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
                                                                               "a664be8d-4359-4e6a-ae92-fa2811aadfe4");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "ebc45cd6-62fd-4f1b-afbc-d5870ae5d620");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "8a420540-ea78-4fe5-8bc7-2132f12211f9");
                                                                        label =
                                                                          [
                                                                            "midterm";
                                                                          ];
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
                                                                               "e6ddeb24-2d51-48e5-8459-4624a85c5478");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "da839b23-86b0-4b26-95c8-3c7ac9dc3601");
                                                                        label =
                                                                          [
                                                                            "78.000000";
                                                                          ];
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
                                                                               "5bbca037-6c43-4e43-88fb-a605ccc990ad");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "a2cfc6e5-f436-4ba4-aa99-4bd21a43a8e6");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "e9a3a5bc-3555-4e49-93ca-14d442e0390f");
                                                                        label =
                                                                          [
                                                                            "final";
                                                                          ];
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
                                                                               "b2b09791-8a45-4d76-92c1-c9b32f7c36cd");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "9e2db20b-ce9d-49ec-8b9f-18db1a14622b");
                                                                        label =
                                                                          [
                                                                            "80.000000";
                                                                          ];
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
                                                                     "186ae40a-09d5-4cbf-8392-bad328b3efde");
                                                              label = [ "," ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
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
                                                                     "5cea83a2-a2a7-4c69-8632-93c9578a150c");
                                                              content =
                                                                Whitespace " ";
                                                            };
                                                          Secondary
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "9fd18d44-42a7-469b-aa44-53854666ebb8");
                                                              content =
                                                                Whitespace "\n";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "8144a767-f08a-4800-be91-2dc605e84e5c");
                                                              label =
                                                                [ "("; ")" ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [ Exp ];
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
                                                                               "7e663cad-2178-4bc3-ad0b-13153eeb879d");
                                                                        label =
                                                                          [
                                                                            "student_id";
                                                                          ];
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
                                                                               "d0f644f2-8947-4a03-abf2-c1e2554329f2");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "773f0529-aba5-4580-bc94-1ca778f3f6ec");
                                                                        label =
                                                                          [
                                                                            "4";
                                                                          ];
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
                                                                               "e4386d62-46cf-4733-a2c1-01ab02afc5e5");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "804131f8-58da-47b7-bcb3-8c70a25b1b69");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "05fc98f8-f6e9-41ba-ac30-6c92e7b0bd90");
                                                                        label =
                                                                          [
                                                                            "year";
                                                                          ];
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
                                                                               "718e9fc2-bf86-47e8-b35a-b01ded5a93a6");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "95ac515e-c26f-4bff-b1da-6e9c6f31fd87");
                                                                        label =
                                                                          [
                                                                            "2025";
                                                                          ];
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
                                                                               "dd2ce83f-627a-451e-a34c-884c78d16c1b");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "f03abc60-24f2-44d4-915a-38b353066b80");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "f1867a9b-20fa-4554-b324-c182465243df");
                                                                        label =
                                                                          [
                                                                            "semester";
                                                                          ];
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
                                                                               "6cc5f498-ca60-4852-8b0f-fb2a9f9344f7");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "7e0d63c5-7f2d-4db1-a96c-0e1e24bb5a21");
                                                                        label =
                                                                          [
                                                                            "Spring";
                                                                          ];
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
                                                                               "4ca64fdb-fc35-40b2-b03a-2fdd6eec7472");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "1cd3bd05-85f9-4ca0-a590-71dbc84070d9");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "a0e5cbf8-a127-4c13-bb2a-30a8f08e2b16");
                                                                        label =
                                                                          [
                                                                            "quiz1";
                                                                          ];
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
                                                                               "ab29a7e2-c795-47b7-93ec-90913f1c9e53");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "215da1d8-7ce9-4609-9ca5-1c806b3252c7");
                                                                        label =
                                                                          [
                                                                            "9.000000";
                                                                          ];
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
                                                                               "cb19b14a-b63a-4e1f-8fe9-53b7671cf984");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "abfa0f8f-d103-4160-bf38-d494642870d7");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "90b94a05-64f7-4790-b66a-1b7db70a8ae2");
                                                                        label =
                                                                          [
                                                                            "quiz2";
                                                                          ];
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
                                                                               "98607c92-91c3-4b0c-965a-e2669fc888d1");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "37c39f7c-1f8a-455b-add9-353fcee9b778");
                                                                        label =
                                                                          [
                                                                            "10.000000";
                                                                          ];
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
                                                                               "3237110e-2ceb-441c-aa14-a7c5536bda4c");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "979a994f-a3f3-480c-9433-af516bc45070");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "8d12a822-08f5-4737-9d2e-3c06a1e0ecc6");
                                                                        label =
                                                                          [
                                                                            "midterm";
                                                                          ];
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
                                                                               "0ef31f7e-29a6-49c3-bc66-969db0262e5c");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "1ee572bc-916e-468b-a972-a57b7f3c7ff8");
                                                                        label =
                                                                          [
                                                                            "95.000000";
                                                                          ];
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
                                                                               "3b4cf146-9c2b-416a-bb4a-575059ac1810");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "474d08d7-9240-490a-8cfe-e2ff3fdb8fe8");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "8b40e084-ea77-437e-aa0e-b4f9ee46b3b4");
                                                                        label =
                                                                          [
                                                                            "final";
                                                                          ];
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
                                                                               "2ac76785-2b26-4c8b-8fce-d8a60dfdab18");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "aafb1dfe-8ad2-4061-b11e-e56bdf2df373");
                                                                        label =
                                                                          [
                                                                            "98.000000";
                                                                          ];
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
                                                                     "2e8c9733-36f6-4a89-9ee1-bcc9083ae3fb");
                                                              label = [ "," ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [];
                                                                  nibs =
                                                                    ( {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
                                                                      },
                                                                      {
                                                                        shape =
                                                                          Concave
                                                                            44;
                                                                        sort =
                                                                          Exp;
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
                                                                     "ad76d556-3c54-4eb1-904d-bb9367587684");
                                                              content =
                                                                Whitespace "\n";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "0117db08-6a9a-4b0a-8725-68ab0224cad0");
                                                              label =
                                                                [ "("; ")" ];
                                                              mold =
                                                                {
                                                                  out = Exp;
                                                                  in_ = [ Exp ];
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
                                                                               "45929be5-4355-4d54-9f40-5adfb764a3c8");
                                                                        label =
                                                                          [
                                                                            "student_id";
                                                                          ];
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
                                                                               "e85b522e-424b-4eb6-8afb-9afc0c121676");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "17fc6eeb-8f94-4882-a7d1-b9e3cbc1426f");
                                                                        label =
                                                                          [
                                                                            "5";
                                                                          ];
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
                                                                               "1c341512-c483-4e8c-9b46-a59f4fe8aa05");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "acf88b15-2b8e-46e7-9b18-a4cba95cd8f4");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "d977fa1d-446c-44c3-a125-1001a234afce");
                                                                        label =
                                                                          [
                                                                            "year";
                                                                          ];
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
                                                                               "0d338eb3-586e-4220-b6e7-3c58ce6fbd00");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "9b013c13-587e-4953-8670-7a688cbd1711");
                                                                        label =
                                                                          [
                                                                            "2024";
                                                                          ];
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
                                                                               "c33cd6ab-02f0-4db6-a65b-a40de2f620f9");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "5bb2d3b2-b09d-47ae-97ef-57be31c9b352");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "3e8c9721-4d1e-424d-a1bd-ce10eb6ea09a");
                                                                        label =
                                                                          [
                                                                            "semester";
                                                                          ];
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
                                                                               "193213d0-b306-435f-9b4f-5131f3dfbac3");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "96e1f06b-f15e-4e62-8023-52a33bb33258");
                                                                        label =
                                                                          [
                                                                            "Fall";
                                                                          ];
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
                                                                               "01825d3e-1acb-47a5-bcd4-37336223457a");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "fb78f3b4-c3ec-40d2-ac87-902b526d7ea8");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "a8b79d77-e5b6-4307-a02e-594b10772a6a");
                                                                        label =
                                                                          [
                                                                            "quiz1";
                                                                          ];
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
                                                                               "e2a102b4-5d54-459e-b0a1-fb1d815db9b7");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "b310cf0c-ff3b-4c06-877e-81508d4852ca");
                                                                        label =
                                                                          [
                                                                            "4.000000";
                                                                          ];
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
                                                                               "81a79658-c7f2-4516-9c88-67d55c406d84");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "9d96686f-83fb-4997-af14-afde0ac04800");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "bd1f008d-40a5-46e0-a5e8-34dc2a06f703");
                                                                        label =
                                                                          [
                                                                            "quiz2";
                                                                          ];
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
                                                                               "b4701e67-b2f5-451d-8066-6a957967df7d");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "c6106205-c568-40ce-9342-18dcb7ec29be");
                                                                        label =
                                                                          [
                                                                            "3.000000";
                                                                          ];
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
                                                                               "204a3784-0970-403c-aee8-1514bfae7eb7");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "355b6b17-639c-41ec-a204-457c0f4d9c37");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "3f3e168d-3ecf-40ad-b00a-3dc3d8e04d06");
                                                                        label =
                                                                          [
                                                                            "midterm";
                                                                          ];
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
                                                                               "4a8fa850-b02f-460a-b43f-1fe8fca31983");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "7e4511db-3af3-4b7c-95ba-3da8395d793a");
                                                                        label =
                                                                          [
                                                                            "60.000000";
                                                                          ];
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
                                                                               "3d6ffde5-3b19-4afa-bba6-740b5f8b5832");
                                                                        label =
                                                                          [
                                                                            ",";
                                                                          ];
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
                                                                                44;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                44;
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
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "dc4732af-dfeb-4cf9-9d56-f5e25b303d06");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Secondary
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "73c38402-14f9-4876-8307-d62c5c87057f");
                                                                        content =
                                                                          Whitespace
                                                                            " ";
                                                                      };
                                                                    Tile
                                                                      {
                                                                        id =
                                                                          Option
                                                                          .get
                                                                            (Haz3lcore
                                                                             .Id
                                                                             .of_string
                                                                               "fb2e555e-25a3-4266-9a12-488380bd13be");
                                                                        label =
                                                                          [
                                                                            "final";
                                                                          ];
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
                                                                               "773cc47c-e3ec-4bd1-bf0a-53a37d9e354a");
                                                                        label =
                                                                          [
                                                                            "=";
                                                                          ];
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
                                                                                39;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                39;
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
                                                                               "6d4c48ba-240d-4776-960d-3465fc134d4e");
                                                                        label =
                                                                          [
                                                                            "65.000000";
                                                                          ];
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
                                                        ];
                                                      ];
                                                  };
                                              ];
                                            ];
                                        };
                                    model = "()";
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "aec7caa3-ddc8-4c3e-bda1-188fbd147743");
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
                                 "cc5c4f0c-3616-4120-9852-70acb2c3f9a3");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "1add448c-02a9-4dda-aec1-4a50b37246eb");
                          label = [ "midterm_mean" ];
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
                                 "3ae134b8-ae1d-4249-bfbe-c67cbcd8aae1");
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
                                           "c87e7d8f-268e-45e5-9b4b-448b28f83365");
                                    label = [ "gradebook" ];
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
                    ] ) );
              ];
          };
        caret = Outer;
        refractors =
          {
            manuals =
              [
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "013a3920-f5c0-44d7-a61f-7aef62590488"),
                  { kind = Probe; model = "((active_renderer()))" } );
              ];
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
                                 "275a4f6a-4b71-43e8-8815-377d21397c30");
                          label = [ "student_id" ];
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
                                 "6562a567-bf27-40c8-8b58-25b8a89494d5");
                          label = [ "=" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 39; sort = Exp },
                                  { shape = Concave 39; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "8b4da0e1-6c1f-477e-b0c0-91650dab03cf");
                          shape = Convex;
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "cfccd438-5b61-479e-abd0-901f78c8bd67");
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
                                 "7dee118b-6369-4bbe-84c2-25f4af738144");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "cdafa25f-2f6a-46fc-9f95-d5256dbefbbe");
                          label = [ "year" ];
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
                                 "f8cb2a99-ab77-4d56-8d51-b9b0214e434b");
                          label = [ "=" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 39; sort = Exp },
                                  { shape = Concave 39; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "f2c9aa8d-1920-4787-971c-fc6807c8bc3d");
                          shape = Convex;
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "a1d6ff5f-3199-4fee-8986-82136321dbba");
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
                                 "6e6974fb-4b89-4b64-8376-8b7e95e6d371");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "93238645-ca82-4172-adb1-c72fdefaad19");
                          label = [ "semester" ];
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
                                 "1eee7922-a047-41e4-9789-acfdd26217bd");
                          label = [ "=" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 39; sort = Exp },
                                  { shape = Concave 39; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "3e31e6cb-21a1-429d-98da-cbafd25c4c2d");
                          shape = Convex;
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "718184d8-f84a-4580-a3b8-272ed52c6884");
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
                                 "56eb1836-fe4a-41f1-8def-f54fbed6cb0b");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "9161d1a8-c7c9-46f6-ac34-a4d291035a0f");
                          label = [ "quiz1" ];
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
                                 "45c6f622-929f-4601-b49e-3a15308aa85e");
                          label = [ "=" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 39; sort = Exp },
                                  { shape = Concave 39; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "d4496a94-06e6-4c4f-ae9c-055763578e03");
                          shape = Convex;
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "3f1900d3-a2e7-4087-89e8-7c5126934e75");
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
                                 "631f03b8-3261-4cb2-a201-5b96c73c59ba");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "25fdab01-301e-41b8-bc2f-83381cb74a4c");
                          label = [ "quiz2" ];
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
                                 "124fd4cc-d6ad-44b4-91b5-1dbf5d8f8ddf");
                          label = [ "=" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 39; sort = Exp },
                                  { shape = Concave 39; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "7b103ad1-2490-4d7c-a7cc-60369de5ee98");
                          shape = Convex;
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "6802fd25-4071-4c3d-8109-020384ee78fa");
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
                                 "3ce8633c-8ad6-477b-a0fc-188c24a4279e");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "f13a1853-fb02-4104-9817-fe4022f2d0f6");
                          label = [ "midterm" ];
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
                                 "59b2e341-40d8-40ab-94b7-c7d1a24f7065");
                          label = [ "=" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 39; sort = Exp },
                                  { shape = Concave 39; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "887ef865-38b5-49e5-be2c-11da2144b674");
                          label = [ "1." ];
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
                                 "6f8fc14b-2215-4ddf-a8de-4e696c89aeba");
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
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "9c3a1487-79d1-4eaf-bc4c-3b561ae46f77");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "0b552c24-a48e-477d-be26-fd2c588e64dd");
                          label = [ "final" ];
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
                                 "4ab457f0-4a35-43e8-a638-6352b40b316e");
                          label = [ "=" ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 39; sort = Exp },
                                  { shape = Concave 39; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "7cdc6948-a734-4f78-b11b-ab326b3b850b");
                          shape = Convex;
                        };
                    ] );
                ancestors =
                  [
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "b2789d58-3ab9-459f-97be-02e5b54ada0d");
                        label = [ "("; ")" ];
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
                                     "34698c22-5f73-4313-8c72-d4ae572c504f");
                              content = Whitespace "\n";
                            };
                        ],
                        [
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "c4f93a88-2640-4359-b1b3-1150aafd2aab");
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
                                     "3659fd34-4fc0-4c15-9ecc-32106a00bd9b");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "862ce1f0-bdbc-4596-bd6d-b47bb5c4de75");
                              label = [ "("; ")" ];
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
                                               "a1e3d07b-d280-4aab-b7b2-e4af18accf4f");
                                        label = [ "student_id" ];
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
                                               "26e65111-f5dd-44ab-a94a-f5e1e688c920");
                                        label = [ "=" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Grout
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "60f640ce-f1a1-480d-859f-25f1a6e40c78");
                                        shape = Convex;
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "970627a7-93bf-4cd9-bf81-2be7497a1401");
                                        label = [ "," ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 44;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 44;
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
                                               "73f9ecbe-5706-491e-8c0d-1591c1e81745");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "26c42e04-4a64-46e1-9ebd-4afb2b5f84ae");
                                        label = [ "year" ];
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
                                               "76fef16d-a385-4f71-963c-822898423a6c");
                                        label = [ "=" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Grout
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "c8c281eb-42be-4b8b-90d9-09154a16402d");
                                        shape = Convex;
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "c8cfac1c-e57b-4fff-929c-bea6054c3664");
                                        label = [ "," ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 44;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 44;
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
                                               "7e927b45-1692-401e-b705-f1c06b85bce3");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "1c29acbf-c852-4702-b2de-3f8dffb6d7c9");
                                        label = [ "semester" ];
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
                                               "365ff1e4-6178-4a23-9e46-64ba12335791");
                                        label = [ "=" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Grout
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "9db159de-c020-4332-9eb3-abad9ea8a02a");
                                        shape = Convex;
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "023b8dde-a759-4792-a745-39bc5934ed43");
                                        label = [ "," ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 44;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 44;
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
                                               "20528dcc-fa4c-4d86-aea7-f9e27c66c9ea");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "1b853ce2-6fb6-415c-ada9-544ba9e25920");
                                        label = [ "quiz1" ];
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
                                               "78ff36d2-578a-4f74-a89e-795f2ec09a90");
                                        label = [ "=" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Grout
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "dfa391b0-63c4-46b1-bdc1-ca887093c508");
                                        shape = Convex;
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "01510864-a8bf-4a90-ad77-25cd5f283e2e");
                                        label = [ "," ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 44;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 44;
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
                                               "4d060817-4d3f-4000-ae2b-4e37196f6bb6");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "07fcb7c6-a655-4b9e-9e0b-9eceb833c56c");
                                        label = [ "quiz2" ];
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
                                               "f5cb8f61-9c79-43e6-9856-aa6dbd94c188");
                                        label = [ "=" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Grout
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e0b96f69-c8f6-4d24-819a-fb3e5e2da43a");
                                        shape = Convex;
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "b7896725-9222-481f-a13f-c7fb22e916ce");
                                        label = [ "," ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 44;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 44;
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
                                               "731b1d7f-a30f-4836-b70c-a4f0f97d594d");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "54ec8f93-dc8e-4c67-86a2-c57dfdb1fa1d");
                                        label = [ "midterm" ];
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
                                               "dc9bc134-a183-487a-b931-2710ddd361d3");
                                        label = [ "=" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 39;
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
                                               "60e33b23-5ab1-4ab3-b263-a7944eb5d192");
                                        label = [ "2." ];
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
                                               "2efd3f5a-91a5-4004-a0c4-4fd7502b6f76");
                                        label = [ "," ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 44;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 44;
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
                                               "867ec4e4-f490-4e92-8cfc-2dbb7e8192e8");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "6729b9c5-ebbf-4958-9bdd-e19697515c1e");
                                        label = [ "final" ];
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
                                               "12b68a16-8a88-4fc0-b5ed-2d55a7469462");
                                        label = [ "=" ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 39;
                                                  sort = Exp;
                                                } );
                                          };
                                        shards = [ 0 ];
                                        children = [];
                                      };
                                    Grout
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "73d068c0-5a9e-4a76-95e1-03ae3c8a98c4");
                                        shape = Convex;
                                      };
                                  ];
                                ];
                            };
                          Secondary
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "2cdfccf6-3624-4414-ad17-3ab83fb038ad");
                              content = Whitespace "\n";
                            };
                        ] ) );
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "bcc3380d-5cb6-40f4-8bd1-f9fd485cddb4");
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
                      ([], []) );
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "83bf15ea-9878-4026-ac91-e039564a10ec");
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
                                     "9113ced6-c284-48e0-a873-bf043ebbec61");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "f8e8f290-ae29-4adc-87e6-40d4190c5754");
                              label = [ "midterm_mean" ];
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
                                     "c5ef077e-f936-4e83-8703-ecb6ef61cbc6");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "80653c78-0d34-490f-a1a4-57be7bfc02ac");
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
                                     "d0660637-c680-4723-9f55-f3286bb20beb");
                              content = Whitespace " ";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "2a05ab3d-e570-4f01-b72b-2793d26605f4");
                              label = [ "1.5" ];
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
                                     "7a3145ff-299e-4f42-b916-d749306e22fb");
                              content = Whitespace " ";
                            };
                        ] ) );
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "d6e59e94-3c3d-40fc-94e0-b48dab89f7f7");
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
                                     "5e9c0856-8d43-4aa4-b4d9-d701481e3499");
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
                                               "cdf6cf60-64d2-4d26-97ee-239899efc98d");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "7302efc7-7c68-4e21-9314-0718d0b02613");
                                        label = [ "midterm_mean" ];
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
                                               "0836f228-41f2-49d3-96f4-277324633c1f");
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
                                                         "e7d04025-0dce-4a08-be33-eb83d867f51d");
                                                  label = [ "gradebook" ];
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
                                               "fc4e4745-3746-46ec-bbf6-cef596778c53");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "981fd65d-3d25-4f28-a95e-58ca466c8255");
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
                                               "9b35fee2-e3b7-4a27-9fa3-766ed85cc3a6");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5e94cd77-dacd-43b3-aad6-840a2970978a");
                                        label = [ "81.6" ];
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
                                               "f7fc185e-54b9-4959-add3-a865825f293a");
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
                                     "bfecce0d-105e-4dce-8024-7eb7e52ca42d");
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
                                     "9b49370d-6528-48cc-bcc2-83cdb9d6f62c");
                              content = Whitespace "\n";
                            };
                        ],
                        [] ) );
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
        hints =
          [
            "Example gradebook mean should be 81.6";
            "Make sure you're taking the total and dividing by the length";
          ];
      };
    wrapper = false;
    show_report = true;
    setting_overrides =
      { Tutorial.default_setting_overrides with display_tables = Some true };
  }
