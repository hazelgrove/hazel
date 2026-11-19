let out : string * Haz3lcore.PersistentZipper.t =
  ( "Stepper Filters",
    Haz3lcore.PersistentZipper.of_slide_text
      "# We want to skip over the evaluation of most expressions, ... #\n\
       debug hide($e) in\n\
       # So that we can explicitly stop at some point in program execution. #\n\n\
       # Here is a buggy factorial implementaiton. We know that fac(3) is \
       problematic. #\n\
       let fac : Int -> Int =\n\
       fun n ->\n\
       case n\n\
       | 0 => 0\n\
       | n => n * fac(n - 1)\n\
       end\n\
       in\n\n\
       let stop_at_fac_3 = fun () ->\n\
       # Therefore, we can stop at the step where the program is about to \
       evaluate fac(3): #\n\
       debug step(fac(3)) in\n\
       # We run our debug-expression through the evaluation of fac(5) #\n\
       fac(5)\n\
       # The program will stop at 5 * (4 * fac(3)), and we can take over and \
       start to stepping through the evaluation of fac(3) manually. #\n\
       in\n\n\
       # Now, here is a correctly implemented map function that applies \
       function f over all elements in array xs. #\n\
       let map : ([Int], Int -> Int) -> [Int] =\n\
       fun xs, f ->\n\
       case xs\n\
       | [] => []\n\
       | hd::tl => f(hd)::map(tl, f)\n\
       end\n\
       in\n\
       let square : Int -> Int = fun x -> x * x in\n\n\
       # One can easily verify the function actually does such thing, by ... #\n\
       let stop_at_square = fun () ->\n\
       # Stopping at each application of the function square #\n\
       debug stop(square($v)) in\n\
       map([1, 2, 3], fun x -> square(x))\n\
       # The program will stop at [square(1), square(2), square(3)] #\n\
       in\n\n\
       (\n\
       stop_at_fac_3(),\n\
       stop_at_square()\n\
       )" )
