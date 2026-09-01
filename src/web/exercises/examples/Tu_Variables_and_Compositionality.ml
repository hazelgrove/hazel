let exercise : Tutorial.spec =
  Tutorial.of_persistent
    {
      id = Haz3lcore.Id.v "b4c5d6e7-8901-2345-6abc-def789012345";
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
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             de2dbaac-257a-414b-8133-b0c7b14624f6)(shape \
             Convex))))))(ancestors())))(caret \
             Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
             -1)(pinned_stack())(indicated_call())(time())(seq \
             0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
          backup_text = "";
        };
      hidden_tests =
        {
          tests =
            {
              zipper =
                "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                 Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
                 ad6305d9-25c2-4c46-add1-8e58ece02261)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 b5e4c056-205a-4a12-9a79-ea16f1fd98c6)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8febe53d-70d9-4479-abec-32187018b9c8)(form(Tok \
                 answer))))(Secondary((id \
                 2695fe49-cf3f-420f-a044-2fd2643c8742)(content(Whitespace\" \
                 \"))))(Tile((id \
                 615bbf03-fb32-418a-b6ee-7e8da9c5349c)(form(Compound \
                 Equals))))(Secondary((id \
                 c221e76a-cf40-4ead-a1a6-1219968430fd)(content(Whitespace\" \
                 \"))))(Tile((id \
                 38953fd8-705a-454d-ae10-9ac0be04082e)(form(Tok \
                 7))))(Secondary((id \
                 5f35c24b-a4c0-4bc9-bed8-8e1d61bf0366)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 e031d5f5-47bb-4e7b-a51d-b887e4017b83)(form(Compound \
                 CellJoin))))(Secondary((id \
                 b446e453-314c-4799-b3dc-d1cde47bb3d3)(content(Whitespace\"\\n\")))))((Grout((id \
                 0ac4206c-bd52-428f-84c0-9f407bee2145)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text = "test answer == 7 end;\n";
            };
          hints =
            [ "Make sure the expression after the keyword in is correct." ];
        };
      wrapper = true;
      show_report = false;
    }

let comp_exercise : Tutorial.spec =
  Tutorial.of_persistent
    {
      id = Haz3lcore.Id.v "c5d6e7f8-9012-3456-7abc-def890123456";
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
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             1809cf69-c27a-4aa4-a341-96e4d2d942cc)(shape \
             Convex))))))(ancestors())))(caret \
             Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
             -1)(pinned_stack())(indicated_call())(time())(seq \
             0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
          backup_text = "";
        };
      hidden_tests =
        {
          tests =
            {
              zipper =
                "((selection((focus Left)(content())(mode Normal)(anchor_caret \
                 Outer)(smart_rounded false)))(relatives((siblings(((Tile((id \
                 638f8601-3373-408a-aa10-52e327de0d50)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 fb2518ef-b54b-455d-9194-980735ba4dd0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 872e933b-b35d-420d-9866-e5bb10df660c)(form(Tok \
                 answer))))(Secondary((id \
                 e650eb0f-200f-4501-a242-b1d2b6a10383)(content(Whitespace\" \
                 \"))))(Tile((id \
                 82b47132-6082-43f8-9394-0fdf5d434de5)(form(Compound \
                 Equals))))(Secondary((id \
                 edd26d08-f2f9-496b-a8d5-24d2f20e944b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6125c38f-5283-4d9b-b6aa-10d3250e0ecc)(form(Tok \
                 5))))(Secondary((id \
                 4407cf0a-ebe4-41f9-a389-89daef79e97d)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 72ad86c7-9404-49d7-a69b-1c22f416c8d5)(form(Compound \
                 CellJoin))))(Secondary((id \
                 ffa16099-c1a1-4f65-9172-febf2f64153e)(content(Whitespace\"\\n\")))))((Grout((id \
                 ec2873f5-194e-4e4c-bce0-0ac337a11b9c)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text = "test answer == 5 end;\n";
            };
          hints =
            [
              "Make sure to embed a let expression as shown in the description!";
            ];
        };
      wrapper = true;
      show_report = false;
    }
