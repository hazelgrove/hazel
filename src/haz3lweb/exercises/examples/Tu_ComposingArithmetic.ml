open Haz3lcore

(* let prompt = Tu_ComposingArithmetic_prompt.prompt *)

let exercise : Tutorial.spec =
  {
    title = "Composing Arithmetic Expressions";
    module_name = "Tu_ComposingArithmetic";
    prompt =
      "Arithmetic expressions are built compositionally by combining smaller \
       expressions with operators like addition (`+`) and multiplication \
       (`*`). The smallest arithmetic expressions are number literals, such as \
       `2` and `42`.\n\n\
       For example, enter the expression `2 * 3 + 4 * 5` in the editor below \
       and observe its computed value: `26`. This follows the standard order \
       of operations, where multiplication precedes addition.\n\n\
       Try moving your cursor through the program to see how Hazel visually \
       groups operands according to their operators, making the structure of \
       the expression clear.\n";
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
