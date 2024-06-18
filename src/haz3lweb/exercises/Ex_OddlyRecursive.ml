let prompt = Ex_OddlyRecursive_prompt.prompt

let exercise : Exercise.spec =
  Exercise.transition
    (Exercise.Programming
       {
         (* header =
            { *)
         title = "Oddly Recursive";
         version = 1;
         module_name = "Ex_OddlyRecursive";
         prompt;
         (* }; *)
         point_distribution =
           { test_validation = 1; mutation_testing = 1; impl_grading = 2 };
         prelude =
           "let not : Bool -> Bool =\nfun x ->\nif x then false else true \nin ";
         correct_impl =
           "let odd:Int->Bool =\n\
            fun x ->\n\
            if x < 0 \n\
            then odd(-x) \n\
            else if x == 0 then false \n\
            else not(odd(x-1)) \n\
            in  ";
         your_tests =
           {
             tests = "test not(false) end;\ntest not(not(true)) end; ";
             required = 6;
             provided = 2;
           };
         your_impl = "let odd: Int -> Bool =\nfun n ->   \nin odd";
         hidden_bugs =
           [
             {
               impl = "let odd: Int -> Bool =\nfun x -> false \nin  ";
               hint = "always returns false";
             };
             {
               impl = "let odd: Int -> Bool =\nfun x -> true \nin  ";
               hint = "always returns true";
             };
             {
               impl =
                 "let odd: Int -> Bool =\n\
                  fun x -> if x < 0 then odd(-x) \n\
                  else if x == 0 then true \n\
                  else if x == 1 then true \n\
                  else odd(x - 1) in\n\
                 \ ";
               hint = "incorrect base case";
             };
           ];
         hidden_tests =
           {
             tests =
               "test not(odd(0)) end;\n\
                test odd(1) end;\n\
                test not(odd(2)) end;\n\
                test odd(3) end;\n\
                test not(odd(42)) end; \n\
                test odd(27) end";
             hints = [ "zero" ];
           };
         syntax_tests = [ ("odd is recursive", IsRecursive "odd") ];
       })
