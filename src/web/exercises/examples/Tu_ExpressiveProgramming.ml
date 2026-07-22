let exercise : Tutorial.spec =
  Tutorial.of_persistent
    {
      id = Haz3lcore.Id.v "d0e1f2a3-4567-8901-2abc-def345678901";
      title = "Expressive Programming";
      version = 1;
      module_name = "Tu_ExpressiveProgramming";
      prompt =
        "You might not have realized it, but you wrote your first computer \
         programs in grade school in the form of arithmetic expressions! \n\n\
         For example, enter the program `2 + 2` in the expression editor \
         below. Hazel operates like a calculator, computing the value of your \
         expression by equationally simplifying it (i.e. evaluating it), here \
         to the integer value `4`. The symbol `\226\137\161` is pronounced \
         \"is equivalent to\".";
      display_hint = "Type 2 + 2 in the cell below \240\159\145\135";
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             580ffb4a-bd93-46bc-ae2c-28ec92015722)(shape \
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
                 b0da2800-df22-457c-94ba-3eae104423e8)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 a710e776-c25a-4a69-a4e7-936d83bd8fe3)(content(Whitespace\" \
                 \"))))(Tile((id \
                 80d6ec54-f3c0-49cb-b251-9ff40eac457f)(form(Tok \
                 answer))))(Secondary((id \
                 d29d887f-2c07-431e-9ee7-9d074d65b608)(content(Whitespace\" \
                 \"))))(Tile((id \
                 c3bbef26-1976-4c0d-ae34-413217176008)(form(Compound \
                 Equals))))(Secondary((id \
                 0d8a65cd-f8b7-4968-9e6a-0182dece981b)(content(Whitespace\" \
                 \"))))(Tile((id \
                 2210fb4a-aa05-42f0-b6fd-f0ffb4d648bc)(form(Tok \
                 4))))(Secondary((id \
                 943e00ec-d4c0-427f-856d-37fff46e79f7)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 b45d5cbb-3918-45fa-b19b-4f218dc21a5a)(form(Compound \
                 CellJoin))))(Secondary((id \
                 1a3c6ccc-f71b-4341-96bd-827c29ba40fa)(content(Whitespace\"\\n\")))))((Grout((id \
                 bd1ca6fd-f075-4657-9ae1-d9afa82b8c8d)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text = "test answer == 4 end;\n";
            };
          hints = [ "Reread the question!" ];
        };
      wrapper = true;
      show_report = false;
    }
