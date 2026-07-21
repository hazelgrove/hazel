let exercise : Tutorial.spec =
  Tutorial.transition
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "f6a7b8c9-d012-3456-789a-bcdef0123456");
      title = "Composing Arithmetic Expressions";
      version = 2;
      module_name = "Tu_ComposingArithmetic";
      prompt =
        "Arithmetic expressions are built compositionally by combining smaller \
         expressions with operators like addition (`+`) and multiplication \
         (`*`). The smallest arithmetic expressions are number literals, such \
         as `2` and `42`.\n\n\
         For example, enter the expression `2 * 3 + 4 * 5` in the editor below \
         and observe its computed value: `26`. This follows the standard order \
         of operations, where multiplication precedes addition.\n\n\
         Try moving your cursor through the program to see how Hazel visually \
         groups operands according to their operators, making the structure of \
         the expression clear.\n";
      display_hint = "";
      your_impl = "";
      hidden_tests =
        {
          tests = "test answer == 26 end;\n";
          hints = [ "Check the expression in the editor!" ];
        };
      wrapper = true;
      show_report = false;
    }

let scope_exercise : Tutorial.spec =
  Tutorial.transition
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "a7b8c9d0-1234-5678-9abc-def012345678");
      title = "Scope";
      version = 6;
      module_name = "Tu_Scope";
      prompt =
        "The scope of a variable is the expression(s) of the program where it \
         is available for use. For let expressions, the sub-expression that \
         follows the `in` keyword has the variable bound by the let expression \
         in scope. Hazel's expression decorations show you where the scope of \
         the variable will end. \n\
        \ 1. Define a variable x inside a let expression.\n\
        \ 2. Assign it the value 5. \n\
        \ 3. Use x in an arithmetic operation inside the in block to return x \
         + 10";
      display_hint = "Add a `+ 10` to your answer from the previous exercise.";
      your_impl = "";
      hidden_tests =
        {
          tests = "test answer == 15 end;\n";
          hints = [ "Make sure you use x as your variable" ];
        };
      wrapper = true;
      show_report = false;
    }
