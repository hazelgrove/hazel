let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "7eab0859-5c92-419a-92cb-54e2c65c6b89");
    title = "Task 3: Gradebook Overall Grade";
    version = 1;
    module_name = "Ta_GradebookOverallGrade";
    prompt =
      "Implement the `clean_and_add_overall_grade` function. It takes a \
       gradebook\n\
       (a list of labeled tuples where every value is a String) and should \
       return\n\
       a new table with two changes:\n\n\
       **Part 1: Convert columns to proper types**\n\
       ```hazelnostatics\n\
       ^^table([\n\
       (column=student_id, `type`=Int),\n\
       (column=term, `type`=String),\n\
       (column=quiz1, `type`=Float),\n\
       (column=quiz2, `type`=Float),\n\
       (column=midterm, `type`=Float),\n\
       (column=final, `type`=Float)\n\
       ])\n\
       ```\n\
       The `term` column should be left unaltered as a String. Preserve the \
       original column order.\n\n\
       **Part 2: Add an `overall_grade` column**\n\n\
       Weighting: 1/3 quizzes, 1/3 midterm, 1/3 final.\n\
       - There are 2 quizzes, each scored out of 10 points.\n\
       - The Midterm and Final are each scored out of 100 points.\n\
       - First convert the quizzes to a percentage:\n\
       quiz_pct = (quiz1 + quiz2) / 20 * 100\n\
       - Then compute:\n\
       overall_grade = (quiz_pct + midterm + final) / 3\n\
       - The final result should be a number out of 100.\n\n\
       **Example:**\n\
       quiz1=8, quiz2=9, midterm=84, final=92\n\
       quiz_pct = (8+9) / 20 * 100 = 85\n\
       overall_grade = (85 + 84 + 92) / 3 = 87\n\n\
       Feel free to define any helper functions you may find useful. The task\n\
       reference to the right provides functions and operations we think may\n\
       be helpful for this task.";
    display_hint =
      "Use `map` to transform each row. Use tuple extension (`...`) to add the\n\
       overall_grade field to each entry. Remember: quiz_pct = sum of 4 quizzes\n\
       divided by 40 times 100";
    task_reference =
      TaskRefDocs.compose
        [
          TaskRefDocs.column_projection;
          TaskRefDocs.tuple_extension;
          TaskRefDocs.map;
          TaskRefDocs.type_conversions_full;
          TaskRefDocs.float_arithmetic;
          TaskRefDocs.partial_application;
        ];
    your_impl =
      {
        selection = { focus = Left; content = []; mode = Normal };
        relatives =
          {
            siblings =
              ( [],
                [
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "5f6c3dd3-f7b0-48fb-8022-ef5986c4c6b9");
                      label = [ "student_id" ];
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
                             "3e681eac-7a1a-49db-84bb-e9d5f500e6db");
                      label = [ "=" ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 39; sort = Typ },
                              { shape = Concave 39; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "5b6c7b4c-04b7-4500-9d7f-c16481b3e092");
                      label = [ "String" ];
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
                             "3145d69c-7a75-46d6-9c17-5d13ce2584d9");
                      label = [ "," ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 44; sort = Typ },
                              { shape = Concave 44; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "b4f2232b-2626-447e-a3d4-07d7824ea4ee");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "8f9540c6-ec6a-4724-9867-8251abcdec80");
                      label = [ "term" ];
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
                             "068b9ce0-956c-4186-bea4-174257743c1e");
                      label = [ "=" ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 39; sort = Typ },
                              { shape = Concave 39; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "80880549-f25b-409e-9fce-7cd56b84ef9b");
                      label = [ "String" ];
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
                             "1a0bebb3-e7d2-4e0d-a61f-1c6f5e5f5d35");
                      label = [ "," ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 44; sort = Typ },
                              { shape = Concave 44; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "69e4fdfa-f01c-482f-b0e2-e3adb558e8f6");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "14387404-1da3-4623-9025-d7c360765890");
                      label = [ "quiz1" ];
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
                             "faa534b4-c554-47f3-9d6b-98a3d5f92593");
                      label = [ "=" ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 39; sort = Typ },
                              { shape = Concave 39; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "2b6154a0-83dd-49b6-8212-574d0c61ec04");
                      label = [ "String" ];
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
                             "c7a05a85-a0dd-432d-a4e9-28ce66862de1");
                      label = [ "," ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 44; sort = Typ },
                              { shape = Concave 44; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "3dee359b-a0c5-406d-b6bf-76a05d15fc2c");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "438ec1d6-7d07-4b52-9d03-8233ecf54a2f");
                      label = [ "quiz2" ];
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
                             "6686e5fc-abe7-48c1-8b4e-049255dc73fb");
                      label = [ "=" ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 39; sort = Typ },
                              { shape = Concave 39; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "c8100c3e-373c-4deb-8117-b960f57f72fa");
                      label = [ "String" ];
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
                             "3314cd77-bd22-45aa-a327-4058bf8e7267");
                      label = [ "," ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 44; sort = Typ },
                              { shape = Concave 44; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "82dbd5fa-3fcc-428c-92ee-ae7c93915737");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "8614d922-b7c7-44a3-9cc0-2ffbc010f0ca");
                      label = [ "midterm" ];
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
                             "f92c76f2-2695-46a6-89fd-66cd76c0b0a5");
                      label = [ "=" ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 39; sort = Typ },
                              { shape = Concave 39; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "5f496b87-99e6-4577-9685-963652dbd3b3");
                      label = [ "String" ];
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
                             "fe1876fc-7fe5-42ce-90cd-7f629957e910");
                      label = [ "," ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 44; sort = Typ },
                              { shape = Concave 44; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Secondary
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "cc28ddb9-cb02-49a3-9577-c49aca84b956");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "4bbdab46-f2ac-40f1-b0e8-fe7bb4c94558");
                      label = [ "final" ];
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
                             "c431767f-61b3-406e-b90e-eff2fcf76866");
                      label = [ "=" ];
                      mold =
                        {
                          out = Typ;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 39; sort = Typ },
                              { shape = Concave 39; sort = Typ } );
                        };
                      shards = [ 0 ];
                      children = [];
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "33e95051-38f9-468b-b24c-3efd98ef4e92");
                      label = [ "String" ];
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
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "6764e169-0b29-4fb0-a772-8022aad87536");
                    label = [ "("; ")" ];
                    mold =
                      {
                        out = Typ;
                        in_ = [ Typ ];
                        nibs =
                          ( { shape = Convex; sort = Typ },
                            { shape = Convex; sort = Typ } );
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
                                 "a8570051-af3d-49ec-b7ed-cdc872c9ad3c");
                          content = Whitespace " ";
                        };
                    ],
                    [
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "dea45c51-e894-4244-949a-af4734ac4878");
                          content = Whitespace " ";
                        };
                    ] ) );
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "f8319f21-946e-4915-9294-79a2e893bf1f");
                    label = [ "type"; "="; "in" ];
                    mold =
                      {
                        out = Exp;
                        in_ = [ TPat; Typ ];
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
                                       "9336bda9-75dc-45dd-9dab-673db73fd2c6");
                                content = Whitespace " ";
                              };
                            Tile
                              {
                                id =
                                  Option.get
                                    (Haz3lcore.Id.of_string
                                       "3f365099-ecd5-4976-ae8d-cb05a9aa1677");
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
                                       "fc183a5a-d67f-4d7e-8eae-76305f2b26fd");
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
                                 "82d22e0f-c1ac-461d-8ede-10d10d6d1fb5");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "dc957200-53ea-4cc3-ab31-8b9e48b1bde4");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "eb6e51ec-736b-4979-8450-c0ddb988338e");
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
                                           "bd8de8a1-6617-4eb6-965f-40e6e8b16414");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "4e60fd0e-8c0e-4650-ad86-0c76deb36ce9");
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
                                           "5425313b-e008-4b89-9e45-152013a43b33");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "8489d17d-7740-4032-bc98-b58e82eccb28");
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
                                           "05a282cd-6a54-403b-afa8-d221f41ffc89");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "2b0acb03-2790-49ae-b28c-552314bfffa4");
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
                                                     "6b512073-4e05-4d39-9084-8d105193b4b9");
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
                                           "9a378521-189d-41bf-8023-07950ace7857");
                                    content = Whitespace " ";
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "738844a6-695f-451f-92d1-23e77ea1e518");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "6d2159f3-9334-4d49-acbd-071881c742cf");
                                    content = Whitespace "\n";
                                  };
                                Projector
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "c66d4579-45b6-4f14-8693-582539f6db32");
                                    kind = Table;
                                    syntax =
                                      Tile
                                        {
                                          id =
                                            Option.get
                                              (Haz3lcore.Id.of_string
                                                 "c66d4579-45b6-4f14-8693-582539f6db32");
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
                                                           "73d7ed31-7c3a-4f7a-b209-31ceb22cab13");
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
                                                                     "12181a14-aed5-4b3c-9bca-5c3477bfb190");
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
                                                                               "3cab3baf-2e3b-4ba6-b224-43fd3f221436");
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
                                                                               "f90038ec-2cb5-43f1-9dfe-fefa62329719");
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
                                                                               "92650a24-feac-4ded-b49e-8216c054ff13");
                                                                        label =
                                                                          [
                                                                            "\"1\"";
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
                                                                               "c85e11a0-9a05-4fd0-afd1-f2b2fca7781b");
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
                                                                               "eb852721-77df-4f7b-afb2-52ac90854cbf");
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
                                                                               "39283038-79b9-4fa5-9c2f-f019f5bece11");
                                                                        label =
                                                                          [
                                                                            "term";
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
                                                                               "bd324ec2-9c41-4151-907b-59c9d07753d6");
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
                                                                               "ef302934-a9fa-4557-aa50-992bf8bc3218");
                                                                        label =
                                                                          [
                                                                            "\"2025SP\"";
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
                                                                               "33ecedac-a9aa-4afd-86f6-e6151dd7e605");
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
                                                                               "66d0a0f5-df22-4ad0-aefe-742bc7f3ed43");
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
                                                                               "8c7a3bb1-95f1-4258-9986-9e0bf348e80a");
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
                                                                               "59d4a4e6-4a77-45ca-af44-fe1011bab7d5");
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
                                                                               "6a64d5af-d12b-403f-b299-52ed6249d273");
                                                                        label =
                                                                          [
                                                                            "\"7\"";
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
                                                                               "6ccd00be-cc10-4213-943a-dc5131f0393a");
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
                                                                               "2019d54e-f497-49af-9488-8668c2a39e21");
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
                                                                               "a6d23b48-894b-4746-9458-e58ed741e2c4");
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
                                                                               "b66fcb9b-8431-407b-8098-5213be711a8d");
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
                                                                               "90240db4-fc23-471f-9af0-ff46494c18de");
                                                                        label =
                                                                          [
                                                                            "\"5\"";
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
                                                                               "a2551b0f-b2bc-4b24-acfa-cc07f5eef0d9");
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
                                                                               "8b015951-bb73-4537-902c-8a8bf8c410c1");
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
                                                                               "ea1948ef-4ab3-44b2-bc9d-d30f43e081a5");
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
                                                                               "06778a97-b8f7-445d-8482-97245dfa415e");
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
                                                                               "148e2660-5026-4ae4-8cb4-24efaca5a39b");
                                                                        label =
                                                                          [
                                                                            "\"85\"";
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
                                                                               "fd84b895-af62-4237-84dd-5e4f947043da");
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
                                                                               "907d7723-aff6-4f0b-9f9f-8fd9d9643294");
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
                                                                               "9769c59e-0d85-4bf8-aa55-b5f6cfeb5ec5");
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
                                                                               "248e2afb-72ec-49b4-a428-470eb71cceac");
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
                                                                               "30874efa-f132-4a38-a5cb-1197eb44655f");
                                                                        label =
                                                                          [
                                                                            "\"88\"";
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
                                                                     "77233198-a571-4502-bc2d-6f722794665c");
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
                                                                     "30a2ce75-4fd5-446b-a185-68b6b40cf911");
                                                              content =
                                                                Whitespace "\n";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "3c90fc7a-a099-469a-bc97-3effd4ccf809");
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
                                                                               "c4b71ba0-a2c2-4d56-abe4-721e1a56af00");
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
                                                                               "4b000b39-52a8-4ca6-9a91-bb6a07c39e7d");
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
                                                                               "8f9c69c2-73ec-49aa-b962-59f90271ec1b");
                                                                        label =
                                                                          [
                                                                            "\"2\"";
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
                                                                               "e43fa99f-f821-4b16-956d-eb69eee10aab");
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
                                                                               "fb3c6030-f184-4d55-ad61-681397093590");
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
                                                                               "22d9bce6-eada-402a-883f-83db7006be7a");
                                                                        label =
                                                                          [
                                                                            "term";
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
                                                                               "97b69393-1c7a-4f7f-a888-3559b3a92469");
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
                                                                               "1a5e140a-c2fc-4518-be65-da566b8fff2c");
                                                                        label =
                                                                          [
                                                                            "\"2025FA\"";
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
                                                                               "9a1726bb-6f3b-4b9e-88af-1bac42c6cf37");
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
                                                                               "d15c48d9-20ae-4a71-ae24-cbea29729a7d");
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
                                                                               "d4933fe8-6bfc-45f8-ae32-a1414db23543");
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
                                                                               "634d5bc6-9fba-4469-ac60-d607ed2dba40");
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
                                                                               "94d9ef6f-4964-4163-b462-fd01c7faf1ba");
                                                                        label =
                                                                          [
                                                                            "\"5\"";
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
                                                                               "42ef6495-944a-4b8a-ba83-8715883a0c94");
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
                                                                               "4f4f47be-3d98-4566-9cf0-03ecf2769fa8");
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
                                                                               "c257f592-9250-47ad-a2ed-35aa08290daf");
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
                                                                               "44db249d-498a-4591-bd4c-a216b66a61e0");
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
                                                                               "dfc6bbb0-7d4b-4e4b-a219-d03617d49701");
                                                                        label =
                                                                          [
                                                                            "\"8\"";
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
                                                                               "640fd302-0a1b-4c6d-8ea2-73c851af54e0");
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
                                                                               "862e8553-863b-4944-ae2a-b5eb184bdd32");
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
                                                                               "980b1305-bb99-403f-849e-f8af04fb0efe");
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
                                                                               "ccb300c0-76ae-48e6-be8c-17bfa5f4fd83");
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
                                                                               "d4da5142-30cf-4e8e-9410-563b60293d17");
                                                                        label =
                                                                          [
                                                                            "\"90\"";
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
                                                                               "d3bfc483-39dd-4b0e-911b-903bf1e9fc8a");
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
                                                                               "48d6a6ba-319f-4ad6-b064-b5eea1e8aa5b");
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
                                                                               "43f9f15f-c21f-4e72-90d8-54d186cdcdc8");
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
                                                                               "932056b2-b39c-425c-a724-96a9d21fb7ce");
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
                                                                               "9ab7ac99-0813-44d7-99b8-6fd02b27b60e");
                                                                        label =
                                                                          [
                                                                            "\"82\"";
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
                                                                     "2241404f-4cea-42f7-a3ad-bc479fb3adbf");
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
                                                                     "300c6cb4-b11d-453e-8381-dc0283c461de");
                                                              content =
                                                                Whitespace "\n";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "772beb8b-28fc-487b-afc5-eb3f68a9c026");
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
                                                                               "f7f97640-fde9-4329-8ef0-f763992648bb");
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
                                                                               "0210755b-feec-4fa5-8d56-eacf2c00500f");
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
                                                                               "adb8ffb1-c240-4f7a-b126-a503bfa7afd1");
                                                                        label =
                                                                          [
                                                                            "\"3\"";
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
                                                                               "30f5f55f-5110-4238-a621-a226ec00425f");
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
                                                                               "044d5f78-12fc-4eb8-a249-7ca33180427b");
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
                                                                               "b613f631-ecd2-4618-9737-32f2b63d5068");
                                                                        label =
                                                                          [
                                                                            "term";
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
                                                                               "7609cb45-8f57-4ee5-a5d0-85f9009ddd58");
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
                                                                               "b8d8dce6-4d12-4951-bd90-0107c5d954b8");
                                                                        label =
                                                                          [
                                                                            "\"2024FA\"";
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
                                                                               "83a667af-6c69-490e-a7eb-c02189aa34e2");
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
                                                                               "89c6e692-2a69-4089-823c-26ebf836b8be");
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
                                                                               "094c8e25-3c50-41ce-8bf6-5b40869415f3");
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
                                                                               "fec9a364-e5c9-496a-bd98-15ed7113fbd4");
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
                                                                               "54254679-7ab4-40d1-ab17-3bf5a7af8b10");
                                                                        label =
                                                                          [
                                                                            "\"8\"";
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
                                                                               "01371ecc-5e0a-4e5f-ba6c-8981f7363cd4");
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
                                                                               "942833bd-d23b-498d-aca6-842622a5e363");
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
                                                                               "a5ce69a2-5cf3-44dd-bfb4-3eeade9f168a");
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
                                                                               "1063050a-a527-43d3-8744-f4e6c3757d3c");
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
                                                                               "6a7ef693-707a-4c04-bae6-3734bb34dfd8");
                                                                        label =
                                                                          [
                                                                            "\"7\"";
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
                                                                               "41e52ae5-de9a-4a1b-8c03-9bed2b1e9801");
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
                                                                               "1b310521-9148-4d22-b949-608e63ad0320");
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
                                                                               "b37dd389-fa3c-4e75-84db-320b1c3b1b0a");
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
                                                                               "35fe2eed-312f-4e70-97d3-e81e29fbf670");
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
                                                                               "b3950359-cea3-4f7c-b4bc-bcbbfc15f80b");
                                                                        label =
                                                                          [
                                                                            "\"78\"";
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
                                                                               "3b52e12c-55eb-458f-97b0-d4774ff4ae33");
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
                                                                               "cc32bcad-6f35-490e-855b-1a3f92485786");
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
                                                                               "fe7eb141-af84-4406-8fbd-16e4c3796a1a");
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
                                                                               "35de5c6f-0de3-4dae-a0ce-3f8d02f6768d");
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
                                                                               "0b29e435-7a87-42c7-acc4-cc171dec40cf");
                                                                        label =
                                                                          [
                                                                            "\"80\"";
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
                                                                     "646afe75-c21e-400e-b105-440bad057549");
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
                                                                     "df5aa3ea-d941-4533-9281-d078e7522d01");
                                                              content =
                                                                Whitespace "\n";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "3eb3a118-8f7c-4690-a650-61096a465df0");
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
                                                                               "fe65348e-f4d3-4a7b-a018-a16f1e7929dc");
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
                                                                               "9a597eb6-4bc7-43e4-b9b4-0d1a2804b00c");
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
                                                                               "e9b5fce0-254b-466e-8ab0-3bf889b5848c");
                                                                        label =
                                                                          [
                                                                            "\"4\"";
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
                                                                               "5c2d0ebe-c41b-45bd-b867-aab42e9f7be9");
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
                                                                               "ae0c291b-f605-49a0-bbfd-2ac9e649eaf5");
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
                                                                               "ef3c0cba-9046-4e35-827c-72e750ab494f");
                                                                        label =
                                                                          [
                                                                            "term";
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
                                                                               "f5664e2e-47de-427a-b964-254efa22ca1a");
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
                                                                               "1ad1769b-64b1-4f72-af53-7f3126505fed");
                                                                        label =
                                                                          [
                                                                            "\"2025SP\"";
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
                                                                               "9d5c9d24-e383-47de-88f7-8fa9b2e1fa9a");
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
                                                                               "0ec93d44-159c-438b-807a-cd496ed53f96");
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
                                                                               "8bb8dfb5-baff-451c-a211-d9eadcdaf9f4");
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
                                                                               "a8c4f6e4-d17a-4fdf-b99b-a297494aff43");
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
                                                                               "644be862-0569-4c11-a939-5c3a7c0360da");
                                                                        label =
                                                                          [
                                                                            "\"9\"";
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
                                                                               "cf4fec23-6957-4934-8358-02b6d0e4b322");
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
                                                                               "15ab74e8-3d80-47ca-ae94-72080b8c9778");
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
                                                                               "f80eb6e6-2a1a-48bd-b61b-f448579d24a3");
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
                                                                               "0fb412df-7295-45b3-a480-02a31473180c");
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
                                                                               "20d79bec-f09f-47b4-8ea5-4f544c9fd1ee");
                                                                        label =
                                                                          [
                                                                            "\"10\"";
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
                                                                               "fb810d8f-8c93-4552-b213-4de1426a1228");
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
                                                                               "a7c67bcf-7a73-4c49-a2bb-8b14491c415b");
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
                                                                               "a1acac18-c12a-4389-8535-e9390e64ffd0");
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
                                                                               "9e29e0cd-f53e-45fd-ba69-889009c45f34");
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
                                                                               "bf6eecee-7243-4504-b4b4-67fdd4fff79c");
                                                                        label =
                                                                          [
                                                                            "\"95\"";
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
                                                                               "f1cc415a-2d22-4f04-8f98-64a238866afe");
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
                                                                               "5e7f3a5f-cc46-46d0-a17f-a21a109dabfa");
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
                                                                               "d532f8cf-738e-4cca-b46b-580df092520f");
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
                                                                               "c7e23e1b-1052-4156-83bc-fc8c066424ec");
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
                                                                               "a6c5445f-5681-492b-b2d8-67927ebb620f");
                                                                        label =
                                                                          [
                                                                            "\"98\"";
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
                                                                     "c83b5090-9378-40cc-9179-9ad9fd62c1e5");
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
                                                                     "ffabc8de-5cea-4840-86a1-e548a31bf3a3");
                                                              content =
                                                                Whitespace "\n";
                                                            };
                                                          Tile
                                                            {
                                                              id =
                                                                Option.get
                                                                  (Haz3lcore.Id
                                                                   .of_string
                                                                     "c59964b4-c2b0-4b01-b8d8-382610d729b0");
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
                                                                               "d4794e30-2a51-4a34-a06a-9d784fda4560");
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
                                                                               "dbd58041-aeb7-463c-8fd0-9ab72fc1d18a");
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
                                                                               "379bfac5-cfea-460a-8378-a9bbdfb7818a");
                                                                        label =
                                                                          [
                                                                            "\"5\"";
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
                                                                               "502dfa08-2ba4-409b-9733-c83bc233d8df");
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
                                                                               "1cd89969-be2d-44de-873d-892aaf31b73b");
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
                                                                               "c65b8c3b-7552-4f05-a431-a12a083be45c");
                                                                        label =
                                                                          [
                                                                            "term";
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
                                                                               "3435cf61-3a19-4d9f-b066-29db4c4e25b6");
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
                                                                               "7d12a4bc-4257-4499-92e9-a0ca4b557759");
                                                                        label =
                                                                          [
                                                                            "\"2024FA\"";
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
                                                                               "04c82ef3-0434-4967-9b47-49f2df549ad5");
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
                                                                               "4d68068e-3783-4a5f-a739-1407429cf2a4");
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
                                                                               "85cc2de4-bb40-449e-adad-04945a8f7062");
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
                                                                               "ef4e8a21-c7f4-4136-98dc-d8c88b3f1639");
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
                                                                               "259b6acf-eac5-4843-9b99-652ad9a5bcd7");
                                                                        label =
                                                                          [
                                                                            "\"4\"";
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
                                                                               "6339c93c-39b8-4aeb-8289-8f76de500955");
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
                                                                               "a134645f-6cc6-41bc-b364-c3abfb286f9e");
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
                                                                               "cf969e92-17ce-4543-b377-0bdc3837686a");
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
                                                                               "1da4c150-0437-4d67-ad1c-33373fcbe85b");
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
                                                                               "1a8f020e-ae70-4afb-b327-313050a3edcf");
                                                                        label =
                                                                          [
                                                                            "\"3\"";
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
                                                                               "f61f2b34-49a4-4b6f-a002-c26a6b8ae529");
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
                                                                               "f138d31f-caf3-4789-9a3b-5f987b9d46d4");
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
                                                                               "e3aebef9-8390-4ce1-a339-f3b93a1d78e4");
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
                                                                               "e937e1ff-474d-47c4-9e5f-7077d129aeab");
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
                                                                               "5eb102fb-3844-4585-920c-952d03990291");
                                                                        label =
                                                                          [
                                                                            "\"60\"";
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
                                                                               "ca60e0f3-3f50-46e3-a162-a87a6b2af833");
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
                                                                               "f05c18ab-6a46-40c8-9f96-2dcb5104838e");
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
                                                                               "b2648822-b6ef-4257-9d50-1a036e15121f");
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
                                                                               "e6b66468-4944-423d-8497-849604d3bc91");
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
                                                                               "13d646fe-1b4a-4c29-b824-f7d2a80d3720");
                                                                        label =
                                                                          [
                                                                            "\"65\"";
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
                                           "37caaf70-5930-444a-b228-5420794d7a7d");
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
                                 "47cd826c-f2c2-4ae9-97cb-6d037a59068d");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "94cc701d-635e-45ac-ba41-4e679dd74dbc");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "1c4dd292-9c0d-4f63-9d7e-09d877647309");
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
                                           "c485bb7e-b905-4972-a6ab-72d987a596de");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "b6602897-bc28-47b9-a340-4994390c559b");
                                    label = [ "clean_and_add_overall_grade" ];
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
                                           "b0d30c4f-a100-4805-8d6e-d2f7e5345e15");
                                    content = Whitespace " ";
                                  };
                              ];
                              [
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "f2e14f8e-cb40-44be-b2ba-dcb6a8a867c7");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "2b7d6ffb-5243-4102-a4d6-ca37c332c129");
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
                                                     "1fa876ee-821a-421d-a6ee-f9445897d272");
                                              content = Whitespace " ";
                                            };
                                          Tile
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "0665faa1-caf8-4ae6-97db-065e00bb17d8");
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
                                                               "2630c112-cb7c-4827-a81d-32352cf31f5e");
                                                        label = [ "gradebook" ];
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
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "d4545d4c-81fc-4f06-af95-8bb78992ac7c");
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
                                                               "6ee1202b-c0f7-4f39-866b-8de741bd09c5");
                                                        content = Whitespace " ";
                                                      };
                                                    Tile
                                                      {
                                                        id =
                                                          Option.get
                                                            (Haz3lcore.Id
                                                             .of_string
                                                               "f5084500-6668-435d-9df4-287fe8ba26f0");
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
                                                                      (Haz3lcore
                                                                       .Id
                                                                       .of_string
                                                                         "66245ee4-45c3-48a4-9325-0b15fa74d2b6");
                                                                  label =
                                                                    [
                                                                      "GradebookEntry";
                                                                    ];
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
                                          Secondary
                                            {
                                              id =
                                                Option.get
                                                  (Haz3lcore.Id.of_string
                                                     "70784693-b97f-431f-b08c-eae476749b8e");
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
                                           "f68aea29-a799-4466-a6ff-fda5e3e189bf");
                                    content = Whitespace "\n";
                                  };
                                Grout
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "bb616ffd-5a87-4c51-98c4-3649b810dba4");
                                    shape = Convex;
                                  };
                                Secondary
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1c69bba2-3ba1-43f9-a4f4-1e155142c1e9");
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
                                 "4825c6b7-26a8-4fb7-878d-3f1278a20241");
                          content = Whitespace "\n";
                        };
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "36220774-bbe0-4f7f-b19c-e36b621e6f85");
                          content = Whitespace "\n";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "0073e144-e1ec-4fe5-b470-6db1da38ce1d");
                          label = [ "clean_and_add_overall_grade" ];
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
                                 "2556860d-6ef9-4567-88e6-5b147bf299a7");
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
                                           "7151c940-783f-4ff9-bc8b-f114816d8783");
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
                       "90ca120f-de9d-4351-ae37-bb2c469c7225"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "563bfdf6-40ed-42b3-aa44-08dcdfdb4142"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "165cae3c-1208-4009-9198-57ebde046bb8"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "7e868982-2f96-413b-8889-6576c27ab282"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "2a45064b-f2f0-4fcb-aa40-7ea4238d583d"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "068f91ef-b587-4f41-a30a-3b6af9408ba2"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "87ab5823-e34d-4e7c-a80f-2aa0c6e41ad5"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "f789cade-0e85-4922-a6c1-3d9b2d290510"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "8098c486-7bd2-4dc3-840d-45320ed30981"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "719d2f7a-045b-4204-af91-c25388417fc1"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "241b7ad0-146e-4418-a312-609cec6ee10f"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "6c7de6e9-f5b9-47ef-b883-176720347e68"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "4111aab6-8ec4-44b7-985c-ff7bed16b82f"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
                ( Option.get
                    (Haz3lcore.Id.of_string
                       "854eddee-63d6-428f-9a13-a80f5a4fc2e7"),
                  {
                    kind = Probe;
                    model =
                      "((active_renderer(((renderer_id \
                       table)(model_state\"((menu_state()))\")))))";
                  } );
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
                      Secondary
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "fd85e16b-bbfd-40a5-96c2-e9e7403caacd");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "7e4f241c-74a9-4835-9c67-d3ba4f1863b8");
                          label = [ "cleaned" ];
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
                                 "1408cef2-4569-47e7-b51d-b52f6bca27f0");
                          label = [ "." ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 22; sort = Exp },
                                  { shape = Concave 22; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                    ],
                    [
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "9e19b298-7826-45b2-a495-5069c09a7383");
                          label = [ "overall_grade" ];
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
                                 "fe599659-2744-4db3-8299-f35565f02e27");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "df58b0c4-e735-495b-86ba-2f080c438f03");
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
                                 "d06f81a9-f9e3-4a6f-b943-739c056505fc");
                          content = Whitespace " ";
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "4fbef973-6a33-488a-9522-db2b2ef02a42");
                          label = [ "our_cleaned" ];
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
                                 "b83c7e90-196c-4ee3-a277-943b3ddb7e28");
                          label = [ "." ];
                          mold =
                            {
                              out = Exp;
                              in_ = [];
                              nibs =
                                ( { shape = Concave 22; sort = Exp },
                                  { shape = Concave 22; sort = Exp } );
                            };
                          shards = [ 0 ];
                          children = [];
                        };
                      Tile
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "326b7bfe-a0d7-4656-aaf0-eac4572f4640");
                          label = [ "overall_grade" ];
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
                                 "d0ae11b4-fa2d-4294-91bb-52acd88d3ca8");
                          content = Whitespace " ";
                        };
                    ] );
                ancestors =
                  [
                    ( {
                        id =
                          Option.get
                            (Haz3lcore.Id.of_string
                               "0bf72568-a61a-4223-94a9-97649d2b3436");
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
                                     "bab9fbb9-106d-463a-937b-0e255e9976cb");
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
                                               "e2e06a9c-42d4-4563-ad15-8e7a92f6ee51");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "a4969d97-bd73-4373-b5d5-d234fe8e7491");
                                        label = [ "cleaned" ];
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
                                               "46f6684f-73c0-4cbb-9813-966f96e0d71a");
                                        content = Whitespace " ";
                                      };
                                  ];
                                  [
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "197cbcd7-b034-46e8-857f-380e3308ba0e");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "879e6f9e-28ec-4d50-86aa-bb449f72e03b");
                                        label =
                                          [ "clean_and_add_overall_grade" ];
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
                                               "2f124a62-d712-4137-9430-3b9bc58c28f8");
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
                                                         "57c18009-a8d5-4a94-86e5-4b8ff2d54e90");
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
                                               "868e1781-cfdc-403d-aeb0-25fdd5e45be3");
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
                                     "57d4be04-ac1b-461b-b108-dda00b4b182c");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "e54b887c-8227-42e9-b2dc-4762d5ab60f2");
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
                                               "9b850953-6863-4c16-a38f-d6a30ad31dcb");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d93159bc-87a7-419c-8254-5d13b783b82e");
                                        label = [ "our_cleaned" ];
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
                                               "5f997da1-2570-416f-af23-2905538fbf4e");
                                        content = Whitespace " ";
                                      };
                                  ];
                                  [
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ef0a3ddd-5a69-4d4a-9221-b1301622d006");
                                        content = Whitespace "\n";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ba2f031a-b2ac-4c01-bc3b-70a2162470e3");
                                        label = [ "gradebook" ];
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
                                               "e5d210f4-930d-4ffc-b464-ea335a2506a1");
                                        content = Whitespace "\n";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "052dbab7-3211-4bd4-82b7-4ca6791d641d");
                                        label = [ "|>" ];
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
                                               "25d8d7fb-f59f-46c8-966d-e78a1bc49b83");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "72e0b4bb-c1d6-4432-a926-b408caa1b117");
                                        label = [ "map" ];
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
                                               "3111d840-ec5a-451b-8d10-d75d42ddf328");
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
                                                         "f6575662-d340-4d6b-85a7-2010b36f330c");
                                                  label = [ "_" ];
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
                                                         "d5a35c58-1c92-4bde-b1fe-461f9daf4814");
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
                                                         "d0eac12a-d365-44bf-8075-32a88c62d7cc");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "971604e7-9b38-481b-96df-c5a709027210");
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
                                                            shape = Concave 37;
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
                                                                   "a2163cb0-a30f-49aa-ad6c-b77e4aa8c1be");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "0f65abaf-6da1-4e97-ab26-09f5af0301da");
                                                            label = [ "("; ")" ];
                                                            mold =
                                                              {
                                                                out = Pat;
                                                                in_ = [ Pat ];
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
                                                                             "b3d694d1-e93d-4754-bc0e-88e6fbee0fb3");
                                                                      label =
                                                                        [ "r" ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Pat;
                                                                          in_ =
                                                                            [];
                                                                          nibs =
                                                                            ( {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Convex;
                                                                                sort =
                                                                                Pat;
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
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "aa8f907f-ece4-454d-b0bf-f165e1594271");
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
                                                         "187471d5-38fb-4ffd-966a-3fbbe4829092");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "7cd8aa00-9f01-4d9c-b11d-134782f53a1a");
                                                  label = [ "r" ];
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
                                                         "47598a93-7f28-48e3-af06-4aa80058b9d4");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "ec364a26-a98d-4e59-a5ac-0699dc2e6da6");
                                                  label = [ "..." ];
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
                                                         "90b071d0-205e-42ff-bc2f-c33220e01272");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "0885edec-c4e8-46d3-8ca9-98b544438017");
                                                  label = [ "("; ")" ];
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
                                                                   "0366a639-1bc6-4cc8-993d-9b4e86f571d1");
                                                            label =
                                                              [ "student_id" ];
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
                                                                   "687cca0c-2c30-4b50-81ef-c03dd4adc0fb");
                                                            label = [ "=" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          39;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          39;
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
                                                                   "03fd5e46-84f2-4bfa-8bea-39957382796c");
                                                            label =
                                                              [
                                                                "int_of_string";
                                                              ];
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
                                                                   "199fa924-79c6-43e5-ba9c-149f4a92de3f");
                                                            label = [ "("; ")" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [ Exp ];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          23;
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
                                                                             "d65ca53b-dfc4-4a99-af64-27150eb7e7e2");
                                                                      label =
                                                                        [ "r" ];
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
                                                                             "3834b9b4-8ea1-40bb-91c5-282ea5356ba5");
                                                                      label =
                                                                        [ "." ];
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
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                22;
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
                                                                             "832fbd41-6b3f-44f2-a7fa-1a9098da28f5");
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
                                                                ];
                                                              ];
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "70a170cc-b15c-4804-a4f7-92877e057d87");
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
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          44;
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
                                                                   "068fe197-c2d5-4b0d-a29e-1eef55afbdc5");
                                                            content =
                                                              Whitespace "\n";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "d14c8448-7293-41a0-bb2c-bb3f42f81207");
                                                            label = [ "quiz1" ];
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
                                                                   "e01b352b-67e6-4828-8b4d-a80bf33ae231");
                                                            label = [ "=" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          39;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          39;
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
                                                                   "a97e941c-31d3-4ea5-879f-554124962ee4");
                                                            label =
                                                              [
                                                                "float_of_string";
                                                              ];
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
                                                                   "dc678bdf-8fb4-40c3-8c9b-fc763961b608");
                                                            label = [ "("; ")" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [ Exp ];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          23;
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
                                                                             "bb162ecd-0bfe-4f66-b5f1-44945b527958");
                                                                      label =
                                                                        [ "r" ];
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
                                                                             "97a205e7-f197-4d11-b9eb-e541286815be");
                                                                      label =
                                                                        [ "." ];
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
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                22;
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
                                                                             "2b19935e-4968-4cfb-ac81-f79050e50734");
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
                                                                ];
                                                              ];
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "a0e03159-3f45-4fed-9851-e50694a7dbfd");
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
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          44;
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
                                                                   "594f8d8d-9850-47c3-8110-476cc5cf759b");
                                                            content =
                                                              Whitespace "\n";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "6a826255-447d-4c5b-9fc1-2db4dd63bf93");
                                                            label = [ "quiz2" ];
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
                                                                   "b73b7c27-d8bb-417d-a0e2-39281d703a6b");
                                                            label = [ "=" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          39;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          39;
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
                                                                   "adcd170a-3d3e-43df-b022-2d0f3ec2f70a");
                                                            label =
                                                              [
                                                                "float_of_string";
                                                              ];
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
                                                                   "2e6d0cbb-7e56-410a-a085-b7b0dfe1640c");
                                                            label = [ "("; ")" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [ Exp ];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          23;
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
                                                                             "b45af152-b1eb-455d-80a7-eebc58c74456");
                                                                      label =
                                                                        [ "r" ];
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
                                                                             "f7736baf-c528-480b-9102-997e89395266");
                                                                      label =
                                                                        [ "." ];
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
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                22;
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
                                                                             "dc78ed7f-cf34-4c4b-a325-47edef2498c8");
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
                                                                ];
                                                              ];
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "36c8b575-7acb-49bc-aec5-df24d5639a4a");
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
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          44;
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
                                                                   "886d81d7-0390-44d3-b0a6-39c5d673afe5");
                                                            content =
                                                              Whitespace "\n";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "a3dd00ed-cbe2-4550-9d39-8dfc067ca2be");
                                                            label =
                                                              [ "midterm" ];
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
                                                                   "c5340942-c444-40b3-942f-b37d88acd7de");
                                                            label = [ "=" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          39;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          39;
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
                                                                   "31d8b800-ee58-4c88-8737-b4b98e730d81");
                                                            label =
                                                              [
                                                                "float_of_string";
                                                              ];
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
                                                                   "acfa45de-a689-4e5b-8125-a69507a49e13");
                                                            label = [ "("; ")" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [ Exp ];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          23;
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
                                                                             "cb0c6c9e-1752-477b-9a19-c210a06f6a59");
                                                                      label =
                                                                        [ "r" ];
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
                                                                             "d8b345ab-ef79-448d-981b-24a6a112c096");
                                                                      label =
                                                                        [ "." ];
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
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                22;
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
                                                                             "139dd8bc-1d01-4a64-9065-7e2c339e4952");
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
                                                                ];
                                                              ];
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "85709a97-e8fa-4abb-9bc5-6808b3f71bb7");
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
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          44;
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
                                                                   "400068fe-d347-488e-9fa2-37a4b7a4ceb4");
                                                            content =
                                                              Whitespace "\n";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "262724cd-de25-41fd-b53f-5524e2fa0eb4");
                                                            label = [ "final" ];
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
                                                                   "4e1ba734-fc72-41ba-96a1-a2438e067d12");
                                                            label = [ "=" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          39;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          39;
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
                                                                   "f240f4c4-97ad-4ede-a381-c75cbf423ac9");
                                                            label =
                                                              [
                                                                "float_of_string";
                                                              ];
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
                                                                   "4562ab95-aabb-43af-938f-b5837dca1678");
                                                            label = [ "("; ")" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [ Exp ];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          23;
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
                                                                             "8b9819d9-da1f-4493-bba9-badbddbc7457");
                                                                      label =
                                                                        [ "r" ];
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
                                                                             "94abe243-cdd3-4b16-8fcc-31cea02bb4a3");
                                                                      label =
                                                                        [ "." ];
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
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                22;
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
                                                                             "eee585f5-c9e3-496e-8266-063111f85b33");
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
                                                                ];
                                                              ];
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
                                               "67e96c4a-86e8-49cf-bf8c-e8715619f6b7");
                                        content = Whitespace "\n";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f275ecbc-c371-496d-b238-0fa0506fb699");
                                        label = [ "|>" ];
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
                                               "bdcac847-bafc-4222-8a58-432543215c79");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0376e235-4dad-4b4c-8e7b-92acf32ae5b1");
                                        label = [ "map" ];
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
                                               "f159e973-eadb-47c0-801f-fe6b7b5f3ee4");
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
                                                         "eace3317-5c92-47dd-868f-3625201b860c");
                                                  label = [ "_" ];
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
                                                         "71754de8-ebaa-44be-86b8-66e0c5a9dfd4");
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
                                                         "63544540-2b65-4598-9494-d7b0400d913d");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "89901f20-e2bf-43bf-93e7-f448afb5ef9e");
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
                                                            shape = Concave 37;
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
                                                                   "048b0d00-37ef-4f80-84cb-5817bf1f3609");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "0bf99b3d-91de-4250-98f1-7c3e544d1b63");
                                                            label = [ "r" ];
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
                                                                   "0095271e-b68c-447d-936c-234094f13276");
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
                                                         "2534b374-9f73-4b26-8205-5f1c46121de8");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "15f65e9e-f01f-4a74-b4d4-056838610b04");
                                                  label = [ "r" ];
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
                                                         "dd457b7f-0a7f-46d6-900d-a3791468812d");
                                                  label = [ "..." ];
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
                                                         "2ee035d5-2e3b-4fc4-bf0b-1038d1891793");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "60465458-2b0d-4438-ba7a-522970159212");
                                                  label = [ "("; ")" ];
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
                                                                   "2a03508a-8c4b-4a56-93b4-27eb409c69e7");
                                                            label =
                                                              [
                                                                "overall_grade";
                                                              ];
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
                                                                   "cc33f8ca-90ca-4e77-b488-4d62ebd52719");
                                                            label = [ "=" ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          39;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          39;
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
                                                                   "44a1faf0-c971-4eec-8cc3-d17c5574b8a9");
                                                            label = [ "("; ")" ];
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
                                                                             "a6d30c99-0589-404c-819d-eed087ca4ce6");
                                                                      label =
                                                                        [ "r" ];
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
                                                                             "11a85da2-8c01-4d4a-be80-77a978fca2a8");
                                                                      label =
                                                                        [ "." ];
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
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                22;
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
                                                                             "26ef41ef-8504-457c-8fbc-96689b99be21");
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
                                                                  Secondary
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "b5724608-8a92-4f88-a970-5d110312a3b9");
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
                                                                             "513a762a-55d9-4f3f-b377-3c937ddd8915");
                                                                      label =
                                                                        [ "+." ];
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
                                                                                28;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                28;
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
                                                                             "b4bc0b8b-5afb-497f-8c97-e3be9d2e9fac");
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
                                                                             "8dbb0cc0-1d2f-4090-b507-d1142173f4b9");
                                                                      label =
                                                                        [ "r" ];
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
                                                                             "e86d9931-b3de-4018-92ff-6dd24ccd290b");
                                                                      label =
                                                                        [ "." ];
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
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                22;
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
                                                                             "700d0c62-28ef-450a-b490-57c9dd9d6e7e");
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
                                                                  Secondary
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "f28b7f15-532f-4d45-ad3c-2a39675c3768");
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
                                                                             "c0bb1e90-c9ac-40a4-bd04-b9488715f79a");
                                                                      label =
                                                                        [ "+." ];
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
                                                                                28;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                28;
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
                                                                             "cda49f29-6e96-46b4-8b2f-3ad4a22c7a0c");
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
                                                                             "8e56f2a9-67a8-4f64-b0dd-8ead438eacd7");
                                                                      label =
                                                                        [
                                                                          "(";
                                                                          ")";
                                                                        ];
                                                                      mold =
                                                                        {
                                                                          out =
                                                                            Exp;
                                                                          in_ =
                                                                            [
                                                                              Exp;
                                                                            ];
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
                                                                        [ 0; 1 ];
                                                                      children =
                                                                        [
                                                                          [
                                                                            Tile
                                                                              {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "422b866b-edd1-43e4-a443-ac5b35933c9e");
                                                                                label =
                                                                                [
                                                                                "r";
                                                                                ];
                                                                                mold =
                                                                                {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
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
                                                                                [
                                                                                0;
                                                                                ];
                                                                                children =
                                                                                [];
                                                                              };
                                                                            Tile
                                                                              {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "1facc508-bcb5-4cfa-9ce7-9ccb5000b4ff");
                                                                                label =
                                                                                [
                                                                                ".";
                                                                                ];
                                                                                mold =
                                                                                {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                                );
                                                                                };
                                                                                shards =
                                                                                [
                                                                                0;
                                                                                ];
                                                                                children =
                                                                                [];
                                                                              };
                                                                            Tile
                                                                              {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "ad2dd2bd-dee4-4f2b-888f-2c9c600f52d9");
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
                                                                                ( 
                                                                                {
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
                                                                                [
                                                                                0;
                                                                                ];
                                                                                children =
                                                                                [];
                                                                              };
                                                                            Secondary
                                                                              {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "537874a7-ae55-4446-a2a1-9771b7620084");
                                                                                content =
                                                                                Whitespace
                                                                                " ";
                                                                              };
                                                                            Tile
                                                                              {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "78a96381-6e9c-447e-9c7e-920dbc7b36a8");
                                                                                label =
                                                                                [
                                                                                "+.";
                                                                                ];
                                                                                mold =
                                                                                {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                28;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                28;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                                );
                                                                                };
                                                                                shards =
                                                                                [
                                                                                0;
                                                                                ];
                                                                                children =
                                                                                [];
                                                                              };
                                                                            Secondary
                                                                              {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "6be228ac-cf4a-4e65-99b0-61bbc9bf38d3");
                                                                                content =
                                                                                Whitespace
                                                                                " ";
                                                                              };
                                                                            Tile
                                                                              {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "0b3a752e-9855-4821-a70d-0f43629c3ffe");
                                                                                label =
                                                                                [
                                                                                "r";
                                                                                ];
                                                                                mold =
                                                                                {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
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
                                                                                [
                                                                                0;
                                                                                ];
                                                                                children =
                                                                                [];
                                                                              };
                                                                            Tile
                                                                              {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "f0d56612-4865-4f12-81c1-df5fdbb212de");
                                                                                label =
                                                                                [
                                                                                ".";
                                                                                ];
                                                                                mold =
                                                                                {
                                                                                out =
                                                                                Exp;
                                                                                in_ =
                                                                                [];
                                                                                nibs =
                                                                                ( 
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                                },
                                                                                {
                                                                                shape =
                                                                                Concave
                                                                                22;
                                                                                sort =
                                                                                Exp;
                                                                                }
                                                                                );
                                                                                };
                                                                                shards =
                                                                                [
                                                                                0;
                                                                                ];
                                                                                children =
                                                                                [];
                                                                              };
                                                                            Tile
                                                                              {
                                                                                id =
                                                                                Option
                                                                                .get
                                                                                (
                                                                                Haz3lcore
                                                                                .Id
                                                                                .of_string
                                                                                "5a96cac2-ea6e-434c-bb21-bb1feebec2be");
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
                                                                                ( 
                                                                                {
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
                                                                                [
                                                                                0;
                                                                                ];
                                                                                children =
                                                                                [];
                                                                              };
                                                                          ];
                                                                        ];
                                                                    };
                                                                  Secondary
                                                                    {
                                                                      id =
                                                                        Option
                                                                        .get
                                                                          (Haz3lcore
                                                                           .Id
                                                                           .of_string
                                                                             "ad860509-8258-4e89-ab8d-3b40009fcf74");
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
                                                                             "ac9475e7-f0e4-49b4-b7e4-894179c872d3");
                                                                      label =
                                                                        [ "*." ];
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
                                                                                27;
                                                                                sort =
                                                                                Exp;
                                                                              },
                                                                              {
                                                                                shape =
                                                                                Concave
                                                                                27;
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
                                                                             "1b4cd107-0b25-4efb-989c-1647ca15494a");
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
                                                                             "1b95b159-24f7-4ec9-a91f-599e71d58835");
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
                                                                ];
                                                              ];
                                                          };
                                                        Secondary
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "db0608cd-9f16-43b8-8b11-69194abea426");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "665e37ea-6339-4021-a6d6-0f433869ecb8");
                                                            label = [ "/." ];
                                                            mold =
                                                              {
                                                                out = Exp;
                                                                in_ = [];
                                                                nibs =
                                                                  ( {
                                                                      shape =
                                                                        Concave
                                                                          27;
                                                                      sort = Exp;
                                                                    },
                                                                    {
                                                                      shape =
                                                                        Concave
                                                                          27;
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
                                                                   "9916103a-0d71-4535-815a-84725ee9149b");
                                                            content =
                                                              Whitespace " ";
                                                          };
                                                        Tile
                                                          {
                                                            id =
                                                              Option.get
                                                                (Haz3lcore.Id
                                                                 .of_string
                                                                   "38833d33-18c6-4f38-9c57-5c21ec676720");
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
                                            ];
                                          ];
                                      };
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "0c0ef942-d4ec-49a0-a2de-9198338431fc");
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
                                     "3d2450c3-e9fc-4513-816a-8b6c9ab66b7a");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "b27a70a3-93ae-4ae3-b683-8300e8916337");
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
                                               "abbb3afe-1cb2-4a8c-93fc-7ff0f42c9776");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "3e5840b8-db62-4368-805b-6950abcdd96b");
                                        label = [ "cleaned" ];
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
                                               "1811dc4e-bdad-408f-b38f-ddd728694215");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "969e5fff-ac31-4633-a03c-c6b53618d05c");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "566b5a59-cce0-4343-ba08-f37b02ff104a");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "71d7a14c-ec58-4785-ae03-2dd082044d7b");
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
                                               "45d0aa4f-5274-4f06-b33d-847a6696d99b");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "3aca5efb-66bd-4fd8-a5d9-ebcfd17b6bbc");
                                        label = [ "our_cleaned" ];
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
                                               "92edb2b1-a84a-40aa-badc-964132edfdde");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "e0aceab9-cf5b-423d-9c73-601a6a332327");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "ac976746-65c9-4086-bd0a-e7f535b0aa44");
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
                                     "0057751b-00b2-409d-b3bd-c2b8b4b81705");
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
                                     "aa626ba6-d6b5-4469-921b-50fb3378e221");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "6a1afa02-e239-4fdc-85a3-3731bedc7c92");
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
                                               "e71563ce-936a-4fe5-b734-c456a19d43f4");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "fc1a5c40-f3a4-4e2d-bd01-c097c4ad0a61");
                                        label = [ "cleaned" ];
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
                                               "c5eb9263-103b-4a91-b3b9-f9b6feda39de");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "4a0eeef0-c374-4feb-897f-4b645407fdda");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f9fa30d7-60db-4546-a16b-878518b17447");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "224db77e-0574-4060-9617-7e958302b852");
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
                                               "29a871c2-e01c-4236-9d20-c68aab86497c");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "4a9a91f4-70e5-4be9-94b8-69ca2d5b4b0c");
                                        label = [ "our_cleaned" ];
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
                                               "0479a10e-1b12-4699-9e0b-f8014f640f8c");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "4308b21e-7808-4a58-a9fd-3ac51373fa94");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "d359b330-9fb4-43aa-b600-3143f5a1fba5");
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
                                     "5b3ed28c-1349-413b-832e-9e60a6046d64");
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
                                     "6d86f8f7-ea73-436a-8007-4836d1a88b6c");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "81d682b9-3e13-4aad-931a-7b92ad07dfe4");
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
                                               "d0954f2b-6e3b-49a2-a637-b0320289348b");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "df5fe1b3-615a-4d9e-963d-2c65a917a6e0");
                                        label = [ "cleaned" ];
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
                                               "ed3c8701-28cc-4bd1-b571-63d90661cb31");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "b523dfb4-dc52-400c-9a8c-ef0075eb131d");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f249a08a-11e4-481a-8d21-1a1806b93087");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "018674b9-ff1d-4302-805b-0dd7d83ddf81");
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
                                               "51702eba-0cc9-48dc-94b6-3d9b7d6aad47");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "15a737e2-77e6-4600-875a-2af2d7420294");
                                        label = [ "our_cleaned" ];
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
                                               "5fd8c221-d5e7-4c25-8936-73a6a77bff79");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "891c61e6-686a-4e49-a400-c744e37d451c");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "8d6599cd-4512-41be-aa42-a95ea15cbd8d");
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
                                     "95acd153-bbdb-4519-8edc-2c85707f8342");
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
                                     "82992343-7066-423e-9713-bd694fd475eb");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "bb851233-b150-4c89-9932-5f5a9f71d9ec");
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
                                               "91b1e49a-260d-4ccb-b4f3-dfac6795f7a2");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f617081c-7d89-4abf-95aa-15358792f43b");
                                        label = [ "cleaned" ];
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
                                               "b5074953-dabd-4e18-bd0e-b1e1b0d7a6b4");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "86bc474b-a284-4510-a7bd-94a5c1fb6b98");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "f572abaa-a00a-4593-a95c-aa3b4f6ba84c");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "b3344216-0f97-40c2-a479-c08b91c88b07");
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
                                               "4c0101fb-38be-42c2-9d73-e3aee814e1b4");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "3a92d84e-edc6-4f7f-8838-47dce4969d70");
                                        label = [ "our_cleaned" ];
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
                                               "3e809357-c5bf-4541-b3a1-de6f25fa789c");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "32324f94-c16d-4b0d-8cab-56eabd46c3f5");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "057c1ae0-72b5-4681-83cb-a495f448c097");
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
                                     "224d7c0e-9e81-4e23-b55c-b6fefe78e970");
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
                                     "c06c8c5a-f45d-4a76-a6c7-d600800b18e0");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "21e157d6-413c-4ab5-b3a0-ded6bd84302c");
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
                                               "ff80ee44-8038-4532-b125-224de18680a5");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "796289e0-b835-4106-b6ff-b8ce88b562ba");
                                        label = [ "cleaned" ];
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
                                               "24c39a33-3eea-4063-9f32-6d26c229a7c9");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "58d9bfcf-8f1a-4848-9804-3fe408e00bf1");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "78e376f0-6819-48a0-b836-f57bac7ceb4f");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "5bfb85b5-f1a6-4efe-a175-ac8f9255e404");
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
                                               "59e7c41c-0d13-4aac-92eb-9a584201b87b");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "28a4ad1f-2811-4a55-b3a3-9359c63e1018");
                                        label = [ "our_cleaned" ];
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
                                               "e227c0f1-fd16-4795-a180-806e3e17caa5");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "f8363a26-168a-47bb-bf75-cbea1784dedf");
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
                                    Secondary
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "e780fe02-453c-41cf-b1ab-3211f7353982");
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
                                     "ebd1f464-3dc1-4610-9251-dc271154aa2a");
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
                                     "cf1d8a44-42d7-4de3-86f6-cbe6354f51e2");
                              content = Whitespace "\n";
                            };
                        ],
                        [
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "7755960e-245f-4ace-8baf-404b3e9e3aaa");
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
                                     "d9b6979b-69bd-4a20-bd04-340ec9d10c57");
                              content = Whitespace "\n";
                            };
                          Tile
                            {
                              id =
                                Option.get
                                  (Haz3lcore.Id.of_string
                                     "a290a966-2207-4fad-9554-f1eab160df4e");
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
                                               "0254c44c-80a7-403d-a7e0-c21753f0b5a4");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "1b72d4af-4c30-4d60-8923-1a651efa9e3d");
                                        label = [ "to_lvs" ];
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
                                               "24050175-806d-46f9-b2ff-138c2698f442");
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
                                                         "b9ffa38e-610a-476e-a056-93b6d4f14633");
                                                  label = [ "head" ];
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
                                                         "6081698f-ce36-478e-8627-78a9ccf5884a");
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
                                                                   "dd039ef3-88be-4ea6-b453-79c7cdc6c2bd");
                                                            label =
                                                              [ "cleaned" ];
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
                                               "af3efa65-24d2-4634-a18b-ddc1884b5bf3");
                                        label = [ "." ];
                                        mold =
                                          {
                                            out = Exp;
                                            in_ = [];
                                            nibs =
                                              ( {
                                                  shape = Concave 22;
                                                  sort = Exp;
                                                },
                                                {
                                                  shape = Concave 22;
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
                                               "e913e314-f506-4de8-8e76-d75f5863eea7");
                                        label = [ "label" ];
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
                                               "1f018145-d327-4e42-93b5-9451d92a7bcf");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "72253648-657f-4347-bce9-02db7ebf2af6");
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
                                               "77e8ece6-218a-4f16-9f31-8ce2d7e4c552");
                                        content = Whitespace " ";
                                      };
                                    Tile
                                      {
                                        id =
                                          Option.get
                                            (Haz3lcore.Id.of_string
                                               "91c1d95e-61ff-486d-87a7-42d927c956b8");
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
                                                         "70f857cf-9c55-4e0f-be60-5ef454d2fd9f");
                                                  label = [ "\"student_id\"" ];
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
                                                         "f342dfd4-8d9a-4684-983b-1d04bc49ed61");
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
                                                         "0b1a0f1b-e6bb-41ce-a96c-717e0df97c1f");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "74178b6c-6baf-4ad7-94bd-ab0f5d505a2a");
                                                  label = [ "\"term\"" ];
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
                                                         "7ac6b904-56db-40f0-94dc-79ceaa2d9d66");
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
                                                         "ef043d71-ba98-465f-aa7d-534b226152e1");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "40c77508-66c9-4502-9335-4ef6a4a0c070");
                                                  label = [ "\"quiz1\"" ];
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
                                                         "b93da613-0e54-403a-9352-92b98a4708cc");
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
                                                         "117c60dd-35a5-4756-88bc-edc09818995d");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "de83460e-1475-4181-ab98-4fdd332364dc");
                                                  label = [ "\"quiz2\"" ];
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
                                                         "52ac6e31-3b8a-4829-a705-d6fc72a10f4c");
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
                                                         "1e294e91-1e3d-472b-be27-3874726554de");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "dfecf436-0533-4fe2-a7c2-9f9adecb728a");
                                                  label = [ "\"midterm\"" ];
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
                                                         "bea7864b-fb8c-4304-8733-8148eec8754f");
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
                                                         "60793ef7-afbb-4e42-8382-2453260cae24");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "aec0bcbc-55bc-464c-98f0-ca11a7cd01a7");
                                                  label = [ "\"final\"" ];
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
                                                         "f54d5155-cdb9-43ae-8dd9-8efb84929ef1");
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
                                                         "5bfdeb32-80fb-4248-b6a8-394412a79544");
                                                  content = Whitespace " ";
                                                };
                                              Tile
                                                {
                                                  id =
                                                    Option.get
                                                      (Haz3lcore.Id.of_string
                                                         "4e483913-7312-427a-a5df-9542abc69823");
                                                  label =
                                                    [ "\"overall_grade\"" ];
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
                                               "93d8be06-a3df-4f38-8e46-5ed5735090c4");
                                        content = Whitespace " ";
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
            "student_id";
            "quiz1";
            "quiz2";
            "midterm";
            "final";
            "overall grade";
            "column names";
          ];
      };
    wrapper = false;
    show_report = true;
    setting_overrides =
      { Tutorial.default_setting_overrides with rich_probes = Some false };
  }
