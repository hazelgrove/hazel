open Haz3lcore

let exercise : Tutorial.spec =
  {
    title = "Variables";
    module_name = "Tu_Variables";
    prompt =
      "As it turns out, you can enrich arithmetic expressions to go from \
       simple integer computations to general-purpose computations! Over the \
       next several slides, we will see how this works.\n\n\
       Our first step is to introduce a way to abbreviate expressions using \
       variables. Type the expression `let x = 2 * 3 in x + 1` into the \
       expression editor below, observing that it evaluates to `7`. You can \
       insert a new line after the `in` keyword to make the program more \
       idiomatic (i.e. readable).\n\n\
       Variables are given computational meaning simply by substitution. Use \
       the stepper to see how this works.";
    wrapper = true;
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
