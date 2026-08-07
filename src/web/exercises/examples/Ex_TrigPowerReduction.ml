open Haz3lcore

let theorem_source =
  "theorem trig_power_reduction = 1. +. 2. *. sin(x) **. 4. == 7. /. 4. -. \
   cos(2. *. x) +. (1. /. 4.) *. cos(4. *. x) in ?"

let zipper_of_source source =
  match Parser.to_zipper ~root:Exp source with
  | Some zipper -> zipper
  | None -> failwith ("Failed to parse theorem exercise source: " ^ source)

let disable_rule rule_id : Language.CustomMathMode.rule_override =
  { rule_id; enabled = false }

let at_most_once capability_id stage : Language.Axioms.capability_usage_override
    =
  { capability_id; stage; usage = Language.Axioms.AtMostOne }

let exercise : Exercise.t =
  Theorem
    {
      id = Option.get (Id.of_string "9b5a75e6-1162-4d52-b1e9-bbbdf8e53296");
      title = "Trigonometric Power Reduction";
      module_name = "Ex_TrigPowerReduction";
      prompt =
        "Target level: Grades 11-12 (Precalculus). Derive the proposal's \
         trigonometric power-reduction identity for `1 + 2 sin(x)^4`. Choose \
         the trigonometric identities explicitly; the locked Trigonometry \
         Check Result profile handles routine algebra and scalar cleanup.";
      max_points = 10;
      write_out_steps = true;
      math_policy =
        Some
          (ExerciseMathPolicy.make ~id:"case-study-trig-power-reduction"
             ~label:"Trig identity with cleanup"
             ~detail:
               "Choose trigonometric identities while Hazel performs \
                profile-authorized algebraic cleanup."
             ~parent_level:Language.Axioms.Trigonometry
             ~automation_stage:Language.Axioms.MultiStepCheck
             ~rule_overrides:
               [
                 disable_rule "trig.pythagorean_sin_cos";
                 disable_rule "trig.pythagorean_cos_sin";
                 disable_rule "trig.cos_squared_pythagorean";
                 disable_rule "trig.sin_sum";
                 disable_rule "trig.sin_diff";
                 disable_rule "trig.cos_sum";
                 disable_rule "trig.cos_diff";
                 disable_rule "trig.sin_double";
                 disable_rule "trig.sin_double_sum_square";
                 disable_rule "trig.cos_double_cos";
                 disable_rule "trig.sin_half_squared";
                 disable_rule "trig.cos_half_squared";
                 disable_rule "trig.sin_cofunction";
                 disable_rule "trig.cos_cofunction";
                 disable_rule "trig.sin_pi_sub";
                 disable_rule "trig.cos_pi_sub";
                 disable_rule "trig.sin_neg";
                 disable_rule "trig.cos_neg";
                 disable_rule "trig.tan_neg";
               ]
             ~usage_overrides:
               [
                 at_most_once "alg.power_mul" Language.Axioms.MultiStepCheck;
                 at_most_once "trig.sin_squared_pythagorean"
                   Language.Axioms.MultiStepCheck;
                 at_most_once "trig.cos_double_square"
                   Language.Axioms.MultiStepCheck;
                 at_most_once "trig.cos_double_sin"
                   Language.Axioms.MultiStepCheck;
                 at_most_once "trig.sin_squared_double"
                   Language.Axioms.MultiStepCheck;
                 at_most_once "trig.cos_squared_double"
                   Language.Axioms.MultiStepCheck;
               ]
             ());
      prelude = Zipper.init ();
      lemmas = Zipper.init ();
      theorem = zipper_of_source theorem_source;
    }
