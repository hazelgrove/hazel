let prompt = Ex_RecursiveFibonacci_prompt.prompt

let exercise : Exercise.spec =
  Exercise.transition
    {
      title = "Recursive Fibonacci";
      version = 1;
      module_name = "Ex_RecursiveFibonacci";
      prompt;
      point_distribution =
        { test_validation = 1; mutation_testing = 1; impl_grading = 2 };
      prelude = " ";
      correct_impl =
        "let fib: Int -> Int = \n\
         fun x -> \n\
         if x < 2 then 1 \n\
         else fib(x - 1) + fib(x - 2) \n\
         in  ";
      your_tests =
        {
          tests =
            "test true end;\n\
             test true end;\n\
             test true end;\n\
             test true end;\n\
             test true end;\n\
             test true end;  ";
          required = 5;
          provided = 0;
        };
      your_impl = "let fib : Int -> Int = \nfun n ->  \nin  ";
      hidden_bugs =
        [
          {
            impl =
              "let fib: Int -> Int = \n\
               fun x -> \n\
               if x < 1 then 0 \n\
               else if x < 2 then 1 \n\
               else fib(x - 1) + fib(x - 2)  \n\
               in  ";
            hint = "incorrect base cases";
          };
          {
            impl =
              "let fib: Int -> Int = \n\
               fun x -> \n\
               if x < 2 then 1  \n\
               else fib(x - 2) + fib(x - 2) \n\
               in  ";
            hint = "incorrect recursion";
          };
        ];
      hidden_tests =
        {
          tests =
            "test fib(0) == 1 end;\n\
             test fib(1) == 1 end;\n\
             test fib(2) == 2 end;\n\
             test fib(3) == 3 end;\n\
             test fib(4) == 5 end;\n\
             test fib(5) == 8 end;\n\
             test fib(6) == 13 end;\n\
             test fib(7) == 21 end;\n\
             test fib(8) == 34 end;\n\
            \ ";
          hints = [];
        };
      syntax_tests = [ ("fib is recursive", IsRecursive "fib") ];
      derivation =
        [
          "gamma |- b /\\ a";
          "gamma |- b";
          "gamma |- b";
          "gamma |- b /\\ a";
          "gamma |- a /\\ b";
          "gamma |- b";
          "gamma |- b";
          "gamma |- b /\\ a";
          "gamma |- a /\\ b";
        ];
    }
