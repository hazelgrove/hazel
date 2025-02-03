open Haz3lcore

let bools_ex : Tutorial.spec =
  {
    title = "Booleans and Types";
    module_name = "Tu_Booleans_and_Types";
    prompt =
      "So far, we have only seen expressions that evaluate to integers. \
       However, Hazel supports many other types of values, like booleans. \
       There are two boolean values, `true` and `false` Hazel's type system \
       ensures that expressions are used in ways that make sense. Each \
       expression has a type, which predicts the type of its value. The \
       expressions in the previous slides all had type `Int`, which is the \
       type of integers, whereas the boolean values have type `Bool`. You can \
       see the type of the expression your cursor is on in the cursor \
       inspector at the bottom of the screen. The symbol `:`  is pronounced \
       \"has type\". Boolean values can be constructed by using comparison \
       operators like `<`, `==`, and `>` on integers. For example, `2 < 3` \
       evaluates to `true`. Booleans can also be combined using logical and \
       (conjunction), `&&`, and logical or (disjunction), `||`.";
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

let cond_ex : Tutorial.spec =
  {
    title = "Conditional Expressions";
    module_name = "Tu_Conditional_Expressions";
    prompt =
      "Given a boolean expression, we can use it to choose between two \
       expressions using a `conditional expression`. For example, `if 2 < 3 \
       then 4 * 4 else 5 * 5` evaluates to `4`.";
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

let func_ex : Tutorial.spec =
  {
    title = "Functions";
    module_name = "Tu_Functions";
    prompt =
      "Functions are expressions that take other expressions as inputs and \
       produce other expressions as outputs. For example,  `fun x -> x + 1` is \
       a function that takes an integer expression as input and produces an \
       integer expression as output. Functions in Hazel do not themselves have \
       names. Instead, you can use a let expression to name a function.\n\
      \      \n\
      \      You apply a function to an argument expression by using \
       parentheses in the usual way. For example, `let f = fun x -> x + 1 in \
       f(2)` evaluates to `3` . Go through this example in the stepper to see \
       how substitution of the function for the variable standing for the \
       function works.";
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
