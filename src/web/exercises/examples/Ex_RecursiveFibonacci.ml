let exercise : Exercise.t =
  Code
    (CodeExercise.transition
       {
         id =
           Option.get
             (Haz3lcore.Id.of_string "12f5e34d-d211-4332-91e2-815e9e183885");
         title = "Recursive Fibonacci";
         module_name = "Ex_RecursiveFibonacci";
         prompt =
           "Write test cases for, and then implement, a function that \
            recursively determines the nth Fibonacci number. \n\
            `fib(n)` is equivalent to the `n`th Fibonacci number, assuming `n \
            >= 0`.";
         point_distribution =
           { test_validation = 1; mutation_testing = 1; impl_grading = 2 };
         prelude = "";
         correct_impl =
           "let fib: Int -> Int = \n\
            fun x -> \n\
            if x < 2 then 1 \n\
            else fib(x - 1) + fib(x - 2) \n\
            in ";
         your_tests = { tests = ""; required = 5; provided = 0 };
         your_impl = "let fib : Int -> Int = \nfun n -> \nin ";
         hidden_bugs =
           [
             {
               impl =
                 "let fib: Int -> Int = \n\
                  fun x -> \n\
                  if x < 1 then 0 \n\
                  else if x < 2 then 1 \n\
                  else fib(x - 1) + fib(x - 2)  \n\
                  in ";
               hint = "incorrect base cases";
             };
             {
               impl =
                 "let fib: Int -> Int = \n\
                  fun x -> \n\
                  if x < 2 then 1  \n\
                  else fib(x - 2) + fib(x - 2) \n\
                  in ";
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
                test fib(8) == 34 end;\n";
             hints = [];
           };
         syntax_tests = [ ("fib is recursive", IsRecursive "fib") ];
       })
