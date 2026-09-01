let bools_ex : Tutorial.spec =
  Tutorial.of_persistent
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
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             eb5542e7-bd4f-46c0-91ec-1838f020037b)(shape \
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
                 dd93f86c-2088-4744-ba18-8055e81f974e)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 b63afc9d-bbf8-489f-b301-308f094939f4)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c6270aae-ea35-4eeb-81c8-e62c986fad0a)(form(Tok \
                 exp1))))(Secondary((id \
                 65b6ffea-9971-455e-86fa-d79bda497aa3)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 e9730ade-3b3f-4e16-bd99-a632a1bd7ad8)(form(Compound \
                 CellJoin))))(Secondary((id \
                 c72db311-318e-4e79-b8f7-0486b1b0f10b)(content(Whitespace\"\\n\"))))(Tile((id \
                 d6e70706-dab9-479e-9e9e-b6a4822ae06f)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 6c6c61fb-e0eb-40c3-a504-0ffb7e78045c)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2972039-c896-4613-9a64-2e706075aabc)(form(Tok \
                 exp2))))(Secondary((id \
                 8ee87054-d6c1-412b-9573-0b14e4a92c06)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 65054472-5b6b-4ede-8d14-d76212d6d9e7)(form(Compound \
                 CellJoin))))(Secondary((id \
                 eb46d80b-c2dc-4fde-8b52-48c3851cade4)(content(Whitespace\"\\n\"))))(Tile((id \
                 ae536025-26ea-4f4d-b444-ffccb61bdcde)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 92861401-5456-4fbb-83aa-af8757c1ba47)(content(Whitespace\" \
                 \"))))(Tile((id \
                 f354dd7c-9d58-4f88-a5bf-2596b2171819)(form(Compound \
                 Not))))(Tile((id \
                 ff23e05f-4ceb-4b62-88a6-0bfce671ea58)(form(Tok \
                 exp3))))(Secondary((id \
                 42c0990c-af99-4cc9-b599-7fd3e823b438)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 63dc8c25-1683-45a1-8464-71ff67ba273a)(form(Compound \
                 CellJoin))))(Secondary((id \
                 12cc1953-cb3c-4d3c-93f1-dab70a12ee9d)(content(Whitespace\"\\n\"))))(Tile((id \
                 76191a2f-d49d-44c8-9165-28d344fe7ab4)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 bacab749-85d3-49ed-94a7-f7b93484ef74)(content(Whitespace\" \
                 \"))))(Tile((id \
                 34932117-51a1-4113-95a8-fa7f49bac384)(form(Compound \
                 Not))))(Tile((id \
                 5558cbbb-33e8-4cf0-894b-0b5dc36ca55d)(form(Tok \
                 exp4))))(Secondary((id \
                 eb8085f5-61df-43d0-9139-804be844f787)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 4827548b-add7-426f-93c7-f9ac71f6330d)(form(Compound \
                 CellJoin))))(Secondary((id \
                 65405e3b-0f31-4da2-ae17-e8d881569843)(content(Whitespace\"\\n\")))))((Grout((id \
                 795eb1b5-555d-47d9-a609-6bfe61da08f1)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text =
                "test exp1 end;\n\
                 test exp2 end;\n\
                 test !exp3 end;\n\
                 test !exp4 end;\n";
            };
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
  Tutorial.of_persistent
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
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             4171df01-1194-48ab-9b5a-be13efdd9ce2)(shape \
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
                 cf1891e9-e05c-4a40-80ce-fdcb44337226)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 a82f6460-d621-4bbf-a890-e75040fe5fcf)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e5783029-8584-4b3b-80f7-c85f7a92afcd)(form(Tok \
                 answer))))(Secondary((id \
                 481eec38-31e7-42c7-b208-eb5ac0ccbc51)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c851faf0-7036-4e2d-9246-284c5b9e15d9)(form(Compound \
                 Equals))))(Secondary((id \
                 5579c731-5681-42e9-b55f-f0687936efd5)(content(Whitespace\" \
                 \"))))(Tile((id \
                 681e8361-4abf-4317-80b3-41c670cca46b)(form(Tok \
                 81))))(Secondary((id \
                 24c31499-59ff-43f8-8847-de6e2da183f5)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 c6da2ba3-6bbc-4918-ab0d-f2280f10447f)(form(Compound \
                 CellJoin))))(Secondary((id \
                 fcb5d439-f30a-48c3-ae60-4ffbabb21c43)(content(Whitespace\"\\n\")))))((Grout((id \
                 452dbbf8-b0db-4e05-99c3-0ca58472c830)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text = "test answer == 81 end;\n";
            };
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
  Tutorial.of_persistent
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
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             f240a137-809f-4a04-839f-355bd8707310)(shape \
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
                 6f23b1a7-1ad1-4972-b592-4afdb4b46a50)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 5bd48ee4-f66e-4fa9-bdea-843e9aaa7e59)(content(Whitespace\" \
                 \"))))(Tile((id \
                 6bf565b6-f544-45fa-bc98-2084aaf9914a)(form(Tok \
                 answer))))(Secondary((id \
                 9fff0bdd-0f8e-47e2-a219-e1d1c26a7431)(content(Whitespace\" \
                 \"))))(Tile((id \
                 b2c7eb1c-212f-4ae0-b624-d5b058eaa12c)(form(Compound \
                 Equals))))(Secondary((id \
                 f8db5d9f-57ff-43bb-bf2b-eaa0cdc60450)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e4fc6559-fa7a-4b13-8df8-4303cfc10894)(form(Tok \
                 3))))(Secondary((id \
                 ad08f3ae-6ae1-4f2b-bd20-632f78f54c0a)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 9a251775-b888-43e1-9946-91473d05453c)(form(Compound \
                 CellJoin))))(Secondary((id \
                 77ad4190-18d8-407a-bc07-55843710797a)(content(Whitespace\"\\n\")))))((Grout((id \
                 4550480c-a61e-4909-9d88-6c75dd4ea2be)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text = "test answer == 3 end;\n";
            };
          hints = [ "Did you copy in the given expression correctly?" ];
        };
      wrapper = true;
      show_report = false;
    }
