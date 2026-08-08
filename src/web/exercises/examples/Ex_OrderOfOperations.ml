open Haz3lcore

let source = "explore 3 + 4 * 2 ** 2 - 6 / 3 end"

let zipper_of_source source =
  match Parser.to_zipper ~root:Exp source with
  | Some zipper -> zipper
  | None -> failwith ("Failed to parse theorem exercise source: " ^ source)

let exercise : Exercise.t =
  Theorem
    {
      id = Option.get (Id.of_string "c539429c-b55a-454d-ab51-abdcde959f5c");
      title = "Order of Operations";
      module_name = "Ex_OrderOfOperations";
      prompt =
        "Target level: Grade 5. Evaluate the expression one local operation at \
         a time. Work through the power, multiplication or division, and \
         addition or subtraction instead of replacing the entire expression \
         with 17.";
      max_points = 10;
      write_out_steps = true;
      math_policy =
        Some
          (ExerciseMathPolicy.make ~id:"case-study-order-of-operations"
             ~label:"Elementary arithmetic, one step at a time"
             ~detail:
               "Evaluate one local arithmetic operation without collapsing the \
                complete expression."
             ~parent_level:Language.Axioms.Arithmetic
             ~automation_stage:Language.Axioms.Manual
             ~show_next_step_hints:false ());
      prelude = Zipper.init ();
      lemmas = Zipper.init ();
      theorem = zipper_of_source source;
      expected_explore_result = Some (zipper_of_source "17");
    }
