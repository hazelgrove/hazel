let exercise : Exercise.spec =
  {
    id =
      Option.get (Haz3lcore.Id.of_string "7d3f9e28-a415-4b92-8c64-9f23d1678459");
    title = "";
    module_name = "";
    prompt = "";
    point_distribution =
      { test_validation = 0; mutation_testing = 0; impl_grading = 0 };
    prelude =
      {
        root = Exp;
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
                             "f2130f9f-a452-481b-ba9a-c40e0d2346aa");
                      shape = Convex;
                    };
                ],
                [] );
            ancestors = [];
          };
        caret = Outer;
      };
    correct_impl =
      {
        root = Exp;
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
                             "b371b234-c6ab-4e88-a8aa-f42c3fd24d6c");
                      shape = Convex;
                    };
                ],
                [] );
            ancestors = [];
          };
        caret = Outer;
      };
    your_tests =
      {
        tests =
          {
            root = Exp;
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
                                 "0f65226c-718c-4e8e-8aae-a43ef4b2805c");
                          shape = Convex;
                        };
                    ],
                    [] );
                ancestors = [];
              };
            caret = Outer;
          };
        required = 0;
        provided = 0;
      };
    your_impl =
      {
        root = Exp;
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
                             "66465f81-0867-4b8e-ae01-99f667cfbe7f");
                      shape = Convex;
                    };
                ] );
            ancestors = [];
          };
        caret = Outer;
      };
    hidden_bugs = [];
    hidden_tests =
      {
        tests =
          {
            root = Exp;
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
                                 "7e32de55-c4b6-4d48-a672-a2e50ff9f98d");
                          shape = Convex;
                        };
                    ],
                    [] );
                ancestors = [];
              };
            caret = Outer;
          };
        hints = [];
      };
    syntax_tests = [];
  }
