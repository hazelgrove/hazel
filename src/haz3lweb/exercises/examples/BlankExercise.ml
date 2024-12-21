open Haz3lcore

let exercise : Exercise.spec =
  {
    id = Option.get (Id.of_string "7d3f9e28-a415-4b92-8c64-9f23d1678459");
    title = "";
    module_name = "";
    prompt = "";
    point_distribution =
      { test_validation = 0; mutation_testing = 0; impl_grading = 0 };
    prelude =
      {
        selection = { focus = Left; content = []; mode = Normal };
        backpack = [];
        relatives =
          {
            siblings =
              ( [ Grout { id = Id.mk (); shape = Convex } ],
                [ Grout { id = Id.mk (); shape = Convex } ] );
            ancestors = [];
          };
        caret = Outer;
      };
    correct_impl =
      {
        selection = { focus = Left; content = []; mode = Normal };
        backpack = [];
        relatives =
          {
            siblings =
              ( [ Grout { id = Id.mk (); shape = Convex } ],
                [ Grout { id = Id.mk (); shape = Convex } ] );
            ancestors = [];
          };
        caret = Outer;
      };
    your_tests =
      {
        tests =
          {
            selection = { focus = Left; content = []; mode = Normal };
            backpack = [];
            relatives =
              {
                siblings =
                  ( [ Grout { id = Id.mk (); shape = Convex } ],
                    [ Grout { id = Id.mk (); shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
          };
        required = 0;
        provided = 0;
      };
    your_impl =
      {
        selection = { focus = Left; content = []; mode = Normal };
        backpack = [];
        relatives =
          {
            siblings =
              ( [ Grout { id = Id.mk (); shape = Convex } ],
                [ Grout { id = Id.mk (); shape = Convex } ] );
            ancestors = [];
          };
        caret = Outer;
      };
    hidden_bugs = [];
    hidden_tests =
      {
        tests =
          {
            selection = { focus = Left; content = []; mode = Normal };
            backpack = [];
            relatives =
              {
                siblings =
                  ( [ Grout { id = Id.mk (); shape = Convex } ],
                    [ Grout { id = Id.mk (); shape = Convex } ] );
                ancestors = [];
              };
            caret = Outer;
          };
        hints = [];
      };
    syntax_tests = [];
  }
