let exercise : Tutorial.spec =
  Tutorial.transition
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
      your_impl = "";
      hidden_tests =
        {
          tests = "test answer == 4 end;\n";
          hints = [ "Reread the question!" ];
        };
      wrapper = true;
      show_report = false;
    }
