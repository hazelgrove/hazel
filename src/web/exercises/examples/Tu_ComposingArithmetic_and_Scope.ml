let exercise : Tutorial.spec =
  Tutorial.of_persistent
    {
      id = Haz3lcore.Id.v "f6a7b8c9-d012-3456-789a-bcdef0123456";
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
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             3e0b0bdd-d1c5-40cd-af93-16eb6e278b81)(shape \
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
                 b429b3db-3956-4bee-bdbd-9794fb940dd5)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 96c8c65c-484a-48e2-8ba2-fcb8eb504ca2)(content(Whitespace\" \
                 \"))))(Tile((id \
                 8152a0d9-074f-4b6f-9057-5f6d666fe7e5)(form(Tok \
                 answer))))(Secondary((id \
                 5f8091aa-9452-4ef6-84af-f689300b0b60)(content(Whitespace\" \
                 \"))))(Tile((id \
                 be81371e-fe09-4d6f-afc1-a8816f35192e)(form(Compound \
                 Equals))))(Secondary((id \
                 f446117c-3d06-4104-9b9b-a3b164699cca)(content(Whitespace\" \
                 \"))))(Tile((id \
                 4cf6288b-c013-455c-8274-27dbf8285265)(form(Tok \
                 26))))(Secondary((id \
                 2b6792dc-246f-42b9-ab16-30e076645405)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 681c5f6e-1cef-408e-8c7c-01e4f0872733)(form(Compound \
                 CellJoin))))(Secondary((id \
                 c38ea792-63d8-4bdf-a008-14db296a2e15)(content(Whitespace\"\\n\")))))((Grout((id \
                 2ddfc5fc-4acc-4b9a-9d5e-f89a497a7871)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text = "test answer == 26 end;\n";
            };
          hints = [ "Check the expression in the editor!" ];
        };
      wrapper = true;
      show_report = false;
    }

let scope_exercise : Tutorial.spec =
  Tutorial.of_persistent
    {
      id = Haz3lcore.Id.v "a7b8c9d0-1234-5678-9abc-def012345678";
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
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             6b90f3c4-d3d9-48d7-ba3b-ef136f0547b0)(shape \
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
                 480ed55f-f1d7-4d76-929a-13ac85c4a52a)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 d9adc260-dcbd-484a-b072-316359ff1ad0)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0ca7d429-a0f3-4428-bf23-f8f10f61f242)(form(Tok \
                 answer))))(Secondary((id \
                 67db684c-837a-4e54-ad82-c0fe1b460358)(content(Whitespace\" \
                 \"))))(Tile((id \
                 0fa78fdb-0249-4626-9b4f-f5fc1e324cc3)(form(Compound \
                 Equals))))(Secondary((id \
                 7905afbe-9427-4bdf-a1bf-70cfde98f816)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e71544a1-ced2-4dcf-9d07-7a807723b546)(form(Tok \
                 15))))(Secondary((id \
                 72b0bd93-611c-45f3-afdc-769ba7fe7b94)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 8682cbc2-b306-4d78-a33e-551a4c2c4dae)(form(Compound \
                 CellJoin))))(Secondary((id \
                 d3fe7341-bc2a-4d81-913c-f932b259a3f1)(content(Whitespace\"\\n\")))))((Grout((id \
                 ea2659ef-4075-4114-b587-368d80e84215)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text = "test answer == 15 end;\n";
            };
          hints = [ "Make sure you use x as your variable" ];
        };
      wrapper = true;
      show_report = false;
    }
