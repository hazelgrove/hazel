open Haz3lcore

let prelude_source =
  "let a = 3. /. 10. in let f = fun t -> 7. /. 4. -. cos(2. *. t) +. (1. /. \
   4.) *. cos(4. *. t) in ?"

let theorem_source =
  "theorem trig_taylor_approximation = let f1 = D f in let f2 = D f1 in let f3 \
   = D f2 in f(a) +. f1(a) *. (x -. a) +. (f2(a) /. 2.) *. (x -. a) **. 2. +. \
   (f3(a) /. 6.) *. (x -. a) **. 3. == f(a) +. (2. *. sin(2. *. a) -. sin(4. \
   *. a)) *. (x -. a) +. ((4. *. cos(2. *. a) -. 4. *. cos(4. *. a)) /. 2.) *. \
   (x -. a) **. 2. +. (((0. -. 8.) *. sin(2. *. a) +. 16. *. sin(4. *. a)) /. \
   6.) *. (x -. a) **. 3. in ?"

let zipper_of_source source =
  match Parser.to_zipper ~root:Exp source with
  | Some zipper -> zipper
  | None -> failwith ("Failed to parse theorem exercise source: " ^ source)

let exercise : Exercise.t =
  Theorem
    {
      id = Option.get (Id.of_string "353b1491-cb8d-462e-b0d7-28b893a5ebe0");
      title = "Taylor Approximation from a Derivative Chain";
      module_name = "Ex_TrigTaylorApproximation";
      prompt =
        "Target level: Calculus II. Construct the third-order Taylor \
         expression for the proposal's trigonometric function about a = 0.3. \
         Check the three explicit derivative functions in sequence, bind them \
         as f1, f2, and f3, then use those results to assemble the polynomial. \
         Routine calculus and algebra cleanup is checked by the locked \
         Calculus Check Result profile.";
      max_points = 10;
      write_out_steps = true;
      math_policy =
        Some
          (ExerciseMathPolicy.make ~id:"case-study-trig-taylor-approximation"
             ~label:"Calculus approximation"
             ~detail:
               "Compose derivative bindings and polynomial assembly with \
                profile-authorized calculus cleanup."
             ~parent_level:Language.Axioms.Calculus
             ~automation_stage:Language.Axioms.MultiStepCheck ());
      prelude = zipper_of_source prelude_source;
      lemmas = Zipper.init ();
      theorem = zipper_of_source theorem_source;
    }
