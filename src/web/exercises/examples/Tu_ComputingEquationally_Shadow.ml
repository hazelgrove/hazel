let exercise : Tutorial.spec =
  Tutorial.transition
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "b8c9d0e1-2345-6789-0abc-def123456789");
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
      your_impl = "";
      hidden_tests =
        {
          tests = "test answer == 26 end;\n";
          hints = [ "Did you type out the given expression correctly?" ];
        };
      wrapper = true;
      show_report = false;
    }

let shadow_exercise : Tutorial.spec =
  Tutorial.transition
    {
      id =
        Option.get
          (Haz3lcore.Id.of_string "c9d0e1f2-3456-7890-1abc-def234567890");
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
         the expression `let y = 8 in let y = 0 in x` would evaluate to 0. \n\n\
         Now, let's try it. First, define a variable x to any number you want. \
         Then, shadow x to the value 7. Make sure you that in both let \
         expression the variable has the same name. ";
      display_hint = "";
      your_impl = "";
      hidden_tests =
        {
          tests = "test x == 7 end;\n";
          hints = [ "Have you shadowed x to 7?" ];
        };
      wrapper = false;
      show_report = false;
    }
