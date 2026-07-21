let exercise : Exercise.t =
  Theorem
    (TheoremExercise.transition
       {
         id = Haz3lcore.Id.v "f2132f9f-a452-481b-ba9a-c40e7d2346aa";
         title = "Reverse! Reverse!";
         module_name = "Ex_ReverseReverse";
         prompt =
           "Show that this implementation of list reverse is its own inverse \
            (i.e. that `rev` is *involutive*).";
         max_points = 10;
         prelude =
           "let snoc = fun (t, h) ->\n\
            case t\n\
            | [] => h :: []\n\
            | h' :: t => h' :: snoc(t, h)\n\
            end\n\
            in\n\n\
            let rev = fun l ->\n\
            case l\n\
            | [] => []\n\
            | h :: t => snoc(rev(t), h)\n\
            end\n\
            in ";
         lemmas = "";
         theorem =
           "theorem rev_rev =\nforall xs:[Int] -> rev(rev(xs)) == xs\nin ";
       })
