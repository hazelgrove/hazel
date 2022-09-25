let exercises: list(SchoolExercise.spec) = SchoolSettings.exercises;
assert(List.length(exercises) > 0);

let init = (~instructor_mode: bool): Editors.school => {
  (
    0,
    exercises,
    List.nth(exercises, 0) |> SchoolExercise.state_of_spec(~instructor_mode),
  );
};
//TODO: make sure in the final version hidden editors are NOT PLAINTEXT STRINGS
