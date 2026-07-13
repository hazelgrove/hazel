open Haz3lcore

let theorem_source =
  "theorem trig_power_reduction = 1 + 2 * sin(x) ** 4 == 7 / 4 - cos(2 * x) + \
   (1 / 4) * cos(4 * x) in ?"

let zipper_of_source source =
  match Parser.to_zipper ~root:Exp source with
  | Some zipper -> zipper
  | None -> failwith ("Failed to parse theorem exercise source: " ^ source)

let exercise : Exercise.t =
  Theorem
    {
      id = Option.get (Id.of_string "9b5a75e6-1162-4d52-b1e9-bbbdf8e53296");
      title = "Power Trip";
      module_name = "Ex_TrigPowerReduction";
      prompt =
        "Prove the trigonometric power-reduction identity for `1 + 2 * sin(x) \
         ** 4`. Use trig identities and algebraic simplification steps rather \
         than jumping directly to the final expression.";
      max_points = 10;
      write_out_steps = true;
      prelude = Zipper.init ();
      lemmas = Zipper.init ();
      theorem = zipper_of_source theorem_source;
    }
