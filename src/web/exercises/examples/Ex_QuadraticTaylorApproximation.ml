open Haz3lcore

let prelude_source = "use Real in (let f(t : Real) = t ** 2 + 3 * t + 2 in ?)"

let theorem_source =
  "theorem quadratic_taylor_approximation = use Real in (let f1 = D f in let \
   f2 = D f1 in f(0) + f1(0) * x + (f2(0) / 2) * x ** 2 == x ** 2 + 3 * x + 2) \
   in ?"

let zipper_of_source source =
  match Parser.to_zipper ~root:Exp source with
  | Some zipper -> zipper
  | None -> failwith ("Failed to parse theorem exercise source: " ^ source)

let exercise : Exercise.t =
  Theorem
    {
      id = Option.get (Id.of_string "7e0bee27-75dd-47a3-b8a6-37604e2c687b");
      title = "Taylor Approximation, First Steps";
      module_name = "Ex_QuadraticTaylorApproximation";
      prompt =
        "Target level: AP Calculus BC or Calculus II. Build the second-order \
         Taylor polynomial for f(t) = t ** 2 + 3 * t + 2 about 0. Check the \
         first two derivative functions, bind them as f1 and f2, then simplify \
         the three Taylor terms to the target polynomial.";
      max_points = 10;
      write_out_steps = true;
      math_policy =
        Some
          (ExerciseMathPolicy.make
             ~id:"case-study-quadratic-taylor-approximation"
             ~label:"Introductory Taylor approximation"
             ~detail:
               "Check two simple derivative bindings and assemble a \
                second-order Taylor polynomial."
             ~parent_level:Language.Axioms.Calculus
             ~automation_stage:Language.Axioms.MultiStepCheck ());
      prelude = zipper_of_source prelude_source;
      lemmas = Zipper.init ();
      theorem = zipper_of_source theorem_source;
      expected_explore_result = None;
    }
