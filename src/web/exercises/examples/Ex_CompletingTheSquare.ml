open Haz3lcore

let source =
  "theorem completing_the_square = x ** 2 + 6 * x + 5 == (x + 3) ** 2 - 4 in ?"

let zipper_of_source source =
  match Parser.to_zipper ~root:Exp source with
  | Some zipper -> zipper
  | None -> failwith ("Failed to parse theorem exercise source: " ^ source)

let disabled_usage capability_id stage :
    Language.Axioms.capability_usage_override =
  { capability_id; stage; usage = Language.Axioms.Disabled }

let exercise : Exercise.t =
  Theorem
    {
      id = Option.get (Id.of_string "3cb80fba-f0ac-48fa-8bbc-f07437e5dec5");
      title = "Completing the Square";
      module_name = "Ex_CompletingTheSquare";
      prompt =
        "Target level: Grades 10-11 (Algebra II). Rewrite the quadratic in \
         completed-square form. Identify half of the linear coefficient and \
         account for the square that is introduced; the Algebra profile may \
         check the resulting sequence and perform routine scalar cleanup.";
      max_points = 10;
      write_out_steps = true;
      math_policy =
        Some
          (ExerciseMathPolicy.make ~id:"case-study-completing-the-square"
             ~label:"Completing the square"
             ~detail:
               "Use profile-authorized algebraic shape and scalar \
                normalization to construct a completed square."
             ~parent_level:Language.Axioms.Algebra
             ~automation_stage:Language.Axioms.MultiStepCheck
             ~usage_overrides:
               [
                 disabled_usage "alg.factor_polynomial_normalize"
                   Language.Axioms.Manual;
                 disabled_usage "alg.factor_polynomial_normalize"
                   Language.Axioms.MultiStepCheck;
                 disabled_usage "alg.factor_polynomial_normalize"
                   Language.Axioms.AutoEval;
               ]
             ());
      prelude = Zipper.init ();
      lemmas = Zipper.init ();
      theorem = zipper_of_source source;
    }
