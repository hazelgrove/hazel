let exercise : Tutorial.spec =
  Tutorial.of_persistent
    {
      id = Haz3lcore.Id.v "b8c9d0e1-2345-6789-0abc-def123456789";
      title = "Computing Equationally";
      version = 3;
      module_name = "Tu_ComputingEquationally";
      prompt =
        "To prove that `2 * 3 + 4 * 5 \226\137\161 26` in grade school, we \
         would have written out a series of equational steps, each simplifying \
         the expression from the previous step by performing one elementary \
         arithmetic computation at a time.\n\n\
         Use Hazel's stepper by clicking the button to the right of the result \
         below and interactively prove that our evaluation to `26` is correct \
         one elementary arithmetic step at a time. This proof is the essence \
         of computation!";
      display_hint =
        "The stepper toggle is located in the bottom right corner of the cell \
         below \240\159\145\135";
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             a0114b7e-e458-492e-8134-0ed746f9fd39)(shape \
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
                 2a092175-0d2a-4827-8045-d5d4ffe53f37)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 3bfc75a6-ec8e-4692-9ea7-b5c1e67b7db9)(content(Whitespace\" \
                 \"))))(Tile((id \
                 98ed9617-b25a-4c2f-8f30-2adcbc495bd6)(form(Tok \
                 answer))))(Secondary((id \
                 de571655-0600-4dd6-be41-661e3d342299)(content(Whitespace\" \
                 \"))))(Tile((id \
                 dd944d60-a248-47b3-82b0-e558e0aae0c9)(form(Compound \
                 Equals))))(Secondary((id \
                 0a09eb29-9a7f-4575-9528-089271b3fe7a)(content(Whitespace\" \
                 \"))))(Tile((id \
                 a7f807d3-52ee-4a04-9fcb-402ccb2e8e8e)(form(Tok \
                 26))))(Secondary((id \
                 041571de-deef-49ec-81ef-1b80cd0dbc2b)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 ce41e96d-6367-43d9-984f-84483530e4e5)(form(Compound \
                 CellJoin))))(Secondary((id \
                 e783d43e-7e75-458a-b3db-4c48294f35c0)(content(Whitespace\"\\n\")))))((Grout((id \
                 91d7980d-0817-4a8a-8d8b-dcabef6bb104)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text = "test answer == 26 end;\n";
            };
          hints = [ "Did you type out the given expression correctly?" ];
        };
      wrapper = true;
      show_report = false;
    }

let shadow_exercise : Tutorial.spec =
  Tutorial.of_persistent
    {
      id = Haz3lcore.Id.v "c9d0e1f2-3456-7890-1abc-def234567890";
      title = "Shadowing";
      version = 7;
      module_name = "Tu_Shadowing";
      prompt =
        "Once a variable is defined, it cannot be changed. There is no \
         assignment operator in languages based on pure mathematical \
         expressions, where variables are given meaning by substitution, like \
         Hazel. However, it is possible to define a `new variable` that shares \
         the name of a previously bound variable. However, this makes it \
         impossible to refer to the previous binding within the scope of the \
         new binding; we say that variable has been `shadowed`. For example, \
         the expression `let y = 8 in let y = 0 in y` would evaluate to 0. \n\n\
         Now, let's try it. First, define a variable x to any number you want. \
         Then, shadow x to the value 7. Make sure you that in both let \
         expression the variable has the same name. ";
      display_hint = "";
      your_impl =
        {
          zipper =
            "((selection((focus Left)(content())(mode Normal)(anchor_caret \
             Outer)(smart_rounded false)))(relatives((siblings(()((Grout((id \
             8f021435-1748-48e2-bfb8-93832e01b869)(shape \
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
                 20daf94a-905b-464d-9b91-9ec9f4313105)(form(Compound \
                 Test))(shards(0 1))(children(((Secondary((id \
                 23f8b808-7cf2-4808-8738-3a94465930af)(content(Whitespace\" \
                 \"))))(Tile((id \
                 714f0d6b-3c58-4388-bb83-f9e09816d884)(form(Tok \
                 x))))(Secondary((id \
                 936eb0d8-0045-4870-9ab7-deabf7a0d039)(content(Whitespace\" \
                 \"))))(Tile((id \
                 e8484103-91d8-4587-bdb8-112218d33ed1)(form(Compound \
                 Equals))))(Secondary((id \
                 cb2bb466-f01b-419a-a8ba-189cd3335639)(content(Whitespace\" \
                 \"))))(Tile((id \
                 9f19ddc2-27aa-427f-aa0d-111505299d07)(form(Tok \
                 7))))(Secondary((id \
                 7dbcfed8-2c98-4e96-89de-a578018d6cf6)(content(Whitespace\" \
                 \")))))))))(Tile((id \
                 08d2dd0f-5d3a-4571-b77c-250808e2b323)(form(Compound \
                 CellJoin))))(Secondary((id \
                 17ac8ca2-6d64-4dea-8be0-8bad11fa8447)(content(Whitespace\"\\n\")))))((Grout((id \
                 09a9bb26-3b95-4507-9319-ed8f5128207c)(shape \
                 Convex))))))(ancestors())))(caret \
                 Outer)(refractors((manuals())(multis((ids())(suppressed())(ephemerals())))(sample_focus((call_stack())(index \
                 -1)(pinned_stack())(indicated_call())(time())(seq \
                 0)(step_range())(pending_focus())))(autoprobe_target())(pending_probe_cursor()))))";
              backup_text = "test x == 7 end;\n";
            };
          hints = [ "Have you shadowed x to 7?" ];
        };
      wrapper = false;
      show_report = false;
    }
