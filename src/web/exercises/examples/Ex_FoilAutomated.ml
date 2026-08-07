open Haz3lcore

let source =
  "theorem foil_with_cleanup = (2 * x - 3) * (x + 4) == 2 * x ** 2 + 5 * x - \
   12 in ?"

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
      id = Option.get (Id.of_string "8cabb249-724e-4570-99f4-51b107887aa9");
      title = "FOIL with Algebraic Cleanup";
      module_name = "Ex_FoilAutomated";
      prompt =
        "Target level: Grades 8-9 (Algebra I). Expand the two binomials. This \
         exercise allows the algebra profile to combine like terms, simplify \
         scalar products, and clean up each visible algebra step.";
      max_points = 10;
      write_out_steps = true;
      math_policy =
        Some
          (ExerciseMathPolicy.make ~id:"case-study-foil-automated"
             ~label:"FOIL with collection"
             ~detail:
               "Use algebraic expansion with collection and scalar cleanup \
                enabled."
             ~parent_level:Language.Axioms.Algebra
             ~automation_stage:Language.Axioms.Manual
             ~usage_overrides:
               [ disabled_usage "alg.expand_polynomial" Language.Axioms.Manual ]
             ());
      prelude = Zipper.init ();
      lemmas = Zipper.init ();
      theorem = zipper_of_source source;
    }
