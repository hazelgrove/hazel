open Haz3lcore

let prompt = Ex_OddlyRecursive_prompt.prompt

let exercise : Tutorial.spec =
  {
    title = "Untitled";
    (* description = ""; *)
    module_name = "Ex_OddlyRecursive_tutorial";
    prompt;
    wrapper = false;
    version = 1;
    your_impl =
      {
        selection = { focus = Left; content = []; mode = Normal };
        backpack = [];
        relatives =
          {
            siblings = ([ Grout { id = Id.mk (); shape = Convex } ], []);
            ancestors = [];
          };
        caret = Outer;
      };
    hidden_tests =
      {
        tests =
          {
            selection = { focus = Left; content = []; mode = Normal };
            backpack = [];
            relatives =
              {
                siblings = ([ Grout { id = Id.mk (); shape = Convex } ], []);
                ancestors = [];
              };
            caret = Outer;
          };
        hints = [];
      };
  }
