open Haz3lcore

let source =
  "theorem foil_with_cleanup = use Real in ((2 * x - 3) * (x + 4) == 2 * x ** \
   2 + 5 * x - 12) in ?"

let zipper_of_source source =
  match Parser.to_zipper ~root:Exp source with
  | Some zipper -> zipper
  | None -> failwith ("Failed to parse theorem exercise source: " ^ source)

let exercise : Exercise.t =
  Theorem
    {
      id = Option.get (Id.of_string "8cabb249-724e-4570-99f4-51b107887aa9");
      title = "FOIL with Algebraic Cleanup";
      module_name = "Ex_FoilAutomated";
      prompt =
        "Target level: Grades 8-9 (Algebra I). Expand the two binomials. This \
         exercise allows one whole-product expansion, then asks you to combine \
         like terms as a separate visible algebra step.";
      max_points = 10;
      write_out_steps = true;
      math_policy =
        Some
          (ExerciseMathPolicy.make ~id:"case-study-foil-automated"
             ~label:"FOIL with collection"
             ~detail:
               "Use whole-polynomial expansion followed by explicit \
                collection; automatic collection is disabled."
             ~parent_level:Language.Axioms.Algebra
             ~automation_stage:Language.Axioms.Manual
             ~cleanup_overrides:
               [
                 Language.CustomMathMode.
                   { capability_id = "collect.like_terms"; enabled = false };
               ]
             ());
      prelude = Zipper.init ();
      lemmas = Zipper.init ();
      theorem = zipper_of_source source;
      expected_explore_result = None;
    }
