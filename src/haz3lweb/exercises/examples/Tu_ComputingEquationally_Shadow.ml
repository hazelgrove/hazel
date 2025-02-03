open Haz3lcore

let exercise : Tutorial.spec =
  {
    title = "Computing Equationally";
    module_name = "Tu_ComputingEquationally";
    prompt =
      "To prove that `2 * 3 + 4 * 5 ≡ 26` in grade school, we would have \
       written out a series of equational steps, each simplifying the \
       expression from the previous step by performing one elementary \
       arithmetic computation at a time.\n\n\
       Use Hazel's stepper by clicking the button to the right of the result \
       below and interactively prove that our evaluation to `26` is correct \
       one elementary arithmetic step at a time. This proof is the essence of \
       computation!";
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

let shadow_exercise : Tutorial.spec =
  {
    title = "Shadowing";
    module_name = "Tu_Shadowing";
    prompt =
      "Once a variable is defined, it cannot be changed. There is no \
       assignment operator in languages based on pure mathematical \
       expressions, where variables are given meaning by substitution, like \
       Hazel. However, it is possible to define a `new variable` that shares \
       the name of a previously bound variable. However, this makes it \
       impossible to refer to the previous binding within the scope of the new \
       binding; we say that variable has been `shadowed`.";
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
