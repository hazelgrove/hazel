open Virtual_dom.Vdom;

module ExerciseEnv = {
  type node = Node.t;
  let default = Node.text("TODO: prompt");
  let output_header = module_name =>
    "let prompt = " ++ module_name ++ "_prompt.prompt\n";
};

include Haz3lschool.SchoolExercise.F(ExerciseEnv);
