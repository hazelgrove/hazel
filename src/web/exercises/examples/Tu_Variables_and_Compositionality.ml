let exercise : Tutorial.spec =
  Tutorial.transition
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "b4c5d6e7-8901-2345-6abc-def789012345");
      title = "Variables";
      version = 4;
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
      display_hint =
        "Make sure your expression starts with `let` and ends with `in`.";
      your_impl = "";
      hidden_tests =
        {
          tests = "test answer == 7 end;\n";
          hints =
            [ "Make sure the expression after the keyword in is correct." ];
        };
      wrapper = true;
      show_report = false;
    }

let comp_exercise : Tutorial.spec =
  Tutorial.transition
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "c5d6e7f8-9012-3456-7abc-def890123456");
      title = "Compositionality";
      version = 5;
      module_name = "Tu_Compositionality";
      prompt =
        "Let expressions are expressions, just like arithmetic expressions. As \
         we discussed earlier, expressions are constructed compositionally, so \
         we can even make a let expression an operand of an arithmetic \
         operator.  \n\n\
         For example, given the following let expression: `let x = 5 in x`, we \
         can embed it into an arithmetic operation as follows: `(let x = 5 in \
         x) * 2`.\n\n\
         Try embedding a let expression in the following expression: 2 + 3. \
         (You can embed the let expression into either the 2, the 3 or both!)\n";
      display_hint = "";
      your_impl = "";
      hidden_tests =
        {
          tests = "test answer == 5 end;\n";
          hints =
            [
              "Make sure to embed a let expression as shown in the description!";
            ];
        };
      wrapper = true;
      show_report = false;
    }
