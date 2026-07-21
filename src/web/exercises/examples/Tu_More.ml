let bools_ex : Tutorial.spec =
  Tutorial.transition
    {
      id = Haz3lcore.Id.v "e1f2a3b4-5678-9012-3abc-def456789012";
      title = "Booleans and Types";
      version = 8;
      module_name = "Tu_Booleans_and_Types";
      prompt =
        "So far, we have only seen expressions that evaluate to integers. \
         However, Hazel supports many other types of values, like booleans. \
         There are two boolean values, `true` and `false`. \n\
        \ \n\
        \ Hazel's type system ensures that expressions are used in ways that \
         make sense. Each expression has a type, which predicts the type of \
         its value. The expressions in the previous slides all had type `Int`, \
         which is the type of integers, whereas the boolean values have type \
         `Bool`. You can see the type of the expression your cursor is on in \
         the cursor inspector at the bottom of the screen. The symbol `:`  is \
         pronounced \"has type\". \n\
        \ \n\
        \ Boolean values can be constructed by using comparison operators like \
         `<`, `==`, and `>` on integers. For example, `2 < 3` evaluates to \
         `true`. Booleans can also be combined using logical and \
         (conjunction), `&&`, and logical or (disjunction), `||`. \n\
        \ \n\
        \ Now declare 4 variables (exp1, exp2, exp3, exp4), and make exp1 and \
         exp2 evaluate to true, and exp3 and exp4 evaluate to false. Feel free \
         to combine different operators using conjunction or disjunction.";
      display_hint = "";
      your_impl = "";
      hidden_tests =
        {
          tests =
            "test exp1 end;\ntest exp2 end;\ntest !exp3 end;\ntest !exp4 end;\n";
          hints =
            [
              "Have you declared exp1? Remember that it has to evaluate to \
               true.";
              "Have you declared exp2? Remember that it has to evaluate to \
               true.";
              "Have you declared exp3? Remember that it has to evaluate to \
               false.";
              "Have you declared exp4? Remember that it has to evaluate to \
               false.";
            ];
        };
      wrapper = false;
      show_report = false;
    }

let cond_ex : Tutorial.spec =
  Tutorial.transition
    {
      id = Haz3lcore.Id.v "f2a3b4c5-6789-0123-4abc-def567890123";
      title = "Conditional Expressions";
      version = 9;
      module_name = "Tu_Conditional_Expressions";
      prompt =
        "Given a boolean expression, we can use it to choose between two \
         expressions using a `conditional expression`.\n\n\
         For example, `if 2 < 3 then 4 * 4 else 5 * 5` evaluates to `16`.\n\n\
         Write a conditional expression that checks if the number `162 < 165`. \
         If it is, return `162 / 2`, otherwise return `162 * 3 + 1`.";
      display_hint = "";
      your_impl = "";
      hidden_tests =
        {
          tests = "test answer == 81 end;\n";
          hints =
            [
              "Have you followed the same format at the example for your \
               expression?";
            ];
        };
      wrapper = true;
      show_report = false;
    }

let func_ex : Tutorial.spec =
  Tutorial.transition
    {
      id = Haz3lcore.Id.v "a3b4c5d6-7890-1234-5abc-def678901234";
      title = "Functions";
      version = 10;
      module_name = "Tu_Functions";
      prompt =
        "Functions are expressions that take other expressions as inputs and \
         produce other expressions as outputs. For example,  `fun x -> x + 1` \
         is a function that takes an integer expression as input and produces \
         an integer expression as output. Functions in Hazel do not themselves \
         have names. Instead, you can use a `let` expression to name a \
         function. \n\
        \ You apply a function to an argument expression by using parentheses \
         in the usual way. For example, `let f = fun x -> x + 1 in f(2)` \
         evaluates to `3` . Go through this example in the stepper to see how \
         substitution of the function for the variable standing for the \
         function works.";
      display_hint =
        "The stepper toggle is located in the bottom right corner of the cell \
         below \240\159\145\135";
      your_impl = "";
      hidden_tests =
        {
          tests = "test answer == 3 end;\n";
          hints = [ "Did you copy in the given expression correctly?" ];
        };
      wrapper = true;
      show_report = false;
    }
