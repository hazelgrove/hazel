let exercise : Tutorial.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "a0000009-0009-0009-0009-000000000009");
    title = "Type Annotations";
    version = 9;
    module_name = "Tu_TypeAnnotations";
    prompt =
      "Hazel's type system ensures that expressions are used in ways that make \
       sense. You can explicitly annotate an expression with a type using the \
       `:` operator. For example, `(1 : Int)` asserts that `1` has type \
       `Int`.\n\n\
       If the annotation does not match the expression, Hazel will report a \
       *type error*.\n\n\
       The editor below contains `(\"1\" : Int)`, which has a type error: \
       `\"1\"` is a `String`, not an `Int`. You will see the type mismatch \
       highlighted in the editor.\n\n\
       Fix the type error by replacing the string `\"1\"` with the integer \
       `1`. The result should evaluate to `1`.";
    display_hint = "Fix the type error by using an integer instead of a string";
    task_reference =
      "## Quick Reference\n\n\
       ### Type Annotation\n\
       ```hazel\n\
       (42 : Int)\n\
       ```\n\
       ```hazel\n\
       (\"hello\" : String)\n\
       ```\n\
       ```hazel\n\
       (true : Bool)\n\
       ```\n\n\
       ### Type annotations on patterns\n\
       ```hazel\n\
       let x : Int = 42 in\n\
       let y : String = \"hello\" in\n\
       let z : Bool = true in\n\
       (x, y, z)\n\
       ```\n\n\
       ### Basic Types\n\
       `Int`, `Float`, `Bool`, `String`";
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
                             "6367c228-a2f5-4c7a-ad8c-c3032a0531a2");
                      label = [ "\"1\"" ];
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
                             "ae9d3a6e-333d-4a42-8d43-cafdc32f47ff");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "e8b5ce38-97eb-458f-b5a7-9e34bd7665f8");
                      label = [ ":" ];
                      mold =
                        {
                          out = Exp;
                          in_ = [];
                          nibs =
                            ( { shape = Concave 24; sort = Exp },
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
                             "af4108b6-7d95-4668-bb8e-60b0f814d05a");
                      content = Whitespace " ";
                    };
                  Tile
                    {
                      id =
                        Option.get
                          (Haz3lcore.Id.of_string
                             "f1831b7f-79bc-43cd-8e8c-5e52dedb7995");
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
                ] );
            ancestors =
              [
                ( {
                    id =
                      Option.get
                        (Haz3lcore.Id.of_string
                           "7bc90944-86e0-4c22-bbaa-f86270a88635");
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
                  ([], []) );
              ];
          };
        caret = Inner 1;
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
                                 "e89454fa-a327-474a-83a1-ff2bc856ddba");
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
                                           "fc60061b-a090-413f-928b-7ca03ab46f26");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "1ee768a1-0c97-4031-99a6-6d0b9f0dcf53");
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
                                           "101fd244-f4c5-4f43-b216-3c03918e8a53");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "8d41f506-b0f8-4e1d-956b-49b9edc7efbc");
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
                                           "1095b218-08e9-43c4-98a3-2d4b4c51c34e");
                                    content = Whitespace " ";
                                  };
                                Tile
                                  {
                                    id =
                                      Option.get
                                        (Haz3lcore.Id.of_string
                                           "fbadd950-0516-411f-a449-73f09211ccf6");
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
                                           "76c6882e-8323-4171-ac70-4454cac74979");
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
                                 "675c871b-7458-45df-a357-a96f47c13b29");
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
                                 "7754b399-9d2e-4c72-bb54-7c1e6b52ce9d");
                          content = Whitespace "\n";
                        };
                    ],
                    [
                      Grout
                        {
                          id =
                            Option.get
                              (Haz3lcore.Id.of_string
                                 "41cc9c4a-32cd-4cf1-8cae-5e4f98cfb2db");
                          shape = Convex;
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
        hints = [ "Replace the string `\"1\"` with the integer `1`." ];
      };
    wrapper = true;
    show_report = false;
    setting_overrides = Tutorial.default_setting_overrides;
  }
