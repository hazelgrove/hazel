let exercise : Exercise.t =
  Derivation
    (DerivationExercise.transition
       {
         id =
           Option.get
             (Haz3lcore.Id.of_string "2f0137e3-672e-47f8-8493-5a593e1959c3");
         title = "Evaluation Derivation";
         module_name = "Ex_EvaluationDerivation";
         prompt =
           "Derive the following judgement using the evaluation rules for ALF. \
            Substitution should be performed inline.";
         max_points = 10;
         prelude = "";
         setup = "";
         rule_set = ALF;
         trees =
           [
             Node
               ( Just
                   {
                     jdmt =
                       "let isNat = fun a -> a > -1 in if isNat (5 - 3) then 1 \
                        else 2 \\=/ 1";
                     rule = None;
                   },
                 [] );
           ];
       })
