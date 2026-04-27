(* Template for a Code exercise. Copy this file, rename it to match your
   exercise module name (keeping the .ml extension), and edit the arguments
   below.

   After cloning, you should also:
     1. Add the new module to [[src/web/exercises/settings/ExerciseSettings_base.re]].
     2. Launch Hazel in instructor mode, flesh the exercise out in the UI, then
        use "Export Exercise Module" to overwrite this file with the fully
        serialized exercise. *)

let exercise : Exercise.t =
  Code
    (CodeExercise.blank_spec ~title:"TODO: title"
       ~module_name:"BlankCodeExercise"
         (* make sure your file is named <module_name>.ml *)
       ~point_distribution:
         { test_validation = 10; mutation_testing = 40; impl_grading = 50 }
         (* if mutation_testing = 0, no mutation testing box will show up *)
       ~required_tests:5 ~provided_tests:0 ~num_wrong_impls:2)
