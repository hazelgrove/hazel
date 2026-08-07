open Haz3lcore

let source =
  "theorem foil_verbose = (x + 1) * (x + 1) == x * x + x * 1 + 1 * x + 1 * 1 \
   in ?"

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
      id = Option.get (Id.of_string "919a933f-bce0-43c5-bedc-700c5f7db090");
      title = "FOIL, Written Out";
      module_name = "Ex_FoilVerbose";
      prompt =
        "Target level: Grades 8-9 (Algebra I). Starting from the product form \
         of the square, distribute only one sum at a time and finish with all \
         four products written explicitly. Do not collect the two middle terms \
         in this exercise.";
      max_points = 10;
      write_out_steps = true;
      math_policy =
        Some
          (ExerciseMathPolicy.make ~id:"case-study-foil-verbose"
             ~label:"Verbose FOIL"
             ~detail:
               "Permit primitive distribution while disabling whole-product \
                expansion and automatic collection."
             ~parent_level:Language.Axioms.Algebra
             ~automation_stage:Language.Axioms.Manual
             ~rule_overrides:
               [
                 Language.CustomMathMode.
                   { rule_id = "alg.square_of_sum"; enabled = false };
               ]
             ~cleanup_overrides:
               [
                 Language.CustomMathMode.
                   { capability_id = "mul.comm"; enabled = true };
                 Language.CustomMathMode.
                   { capability_id = "collect.like_terms"; enabled = false };
               ]
             ~usage_overrides:
               [
                 disabled_usage "alg.expand_polynomial" Language.Axioms.Manual;
                 disabled_usage "alg.expand_polynomial"
                   Language.Axioms.MultiStepCheck;
                 disabled_usage "alg.expand_polynomial" Language.Axioms.AutoEval;
               ]
             ());
      prelude = Zipper.init ();
      lemmas = Zipper.init ();
      theorem = zipper_of_source source;
    }
