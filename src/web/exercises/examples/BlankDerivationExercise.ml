(* Template for a Derivation exercise. Copy this file, rename it to match
   your exercise module name (keeping the .ml extension), and edit the
   arguments below.

   After cloning, you should also:
     1. Add the new module to [[src/web/exercises/settings/ExerciseSettings_base.re]].
     2. Launch Hazel in instructor mode, flesh the exercise out in the UI, then
        use "Export Exercise Module" to overwrite this file with the fully
        serialized exercise. *)

let exercise : Exercise.t =
  Derivation
    (DerivationExercise.blank_spec ~title:"TODO: title"
       ~module_name:"BlankDerivationExercise"
         (* make sure your file is named <module_name>.ml *))
