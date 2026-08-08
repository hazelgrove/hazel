open Haz3lcore

let source =
  "theorem polynomial_derivative = use Real in (deriv (x ** 3 + 2 * x) by x == \
   3 * x ** 2 + 2) in ?"

let zipper_of_source source =
  match Parser.to_zipper ~root:Exp source with
  | Some zipper -> zipper
  | None -> failwith ("Failed to parse theorem exercise source: " ^ source)

let exercise : Exercise.t =
  Theorem
    {
      id = Option.get (Id.of_string "c7fd6416-6834-4f80-aad7-a43fa97c02f7");
      title = "Polynomial Derivative, Check Each Result";
      module_name = "Ex_PolynomialDerivative";
      prompt =
        "Target level: Grade 12 or introductory calculus. Differentiate the \
         polynomial by checking each meaningful transformation. Apply \
         linearity, then the power and product rules, before performing \
         arithmetic cleanup.";
      max_points = 10;
      write_out_steps = true;
      math_policy =
        Some
          (ExerciseMathPolicy.make ~id:"case-study-polynomial-derivative"
             ~label:"Calculus rules, written out"
             ~detail:
               "Check derivative transformations without automatic basic \
                derivative cleanup."
             ~parent_level:Language.Axioms.Calculus
             ~automation_stage:Language.Axioms.MultiStepCheck
             ~cleanup_overrides:
               [
                 Language.CustomMathMode.
                   { capability_id = "derivative.basics"; enabled = false };
               ]
             ());
      prelude = Zipper.init ();
      lemmas = Zipper.init ();
      theorem = zipper_of_source source;
      expected_explore_result = None;
    }
