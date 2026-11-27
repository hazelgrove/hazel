open Alcotest;
open Web;

module Mode = TheoremExerciseMode;

let model = (~write_out_steps) =>
  Mode.Model.of_spec({
    ...TheoremExercise.blank_spec(~title="t", ~module_name="T"),
    write_out_steps,
  });

let settings = (~instructor_mode=false, write_out_steps): Settings.t => {
  let init = Settings.Model.init;
  {
    ...init,
    instructor_mode,
    core: {
      ...init.core,
      evaluation: {
        ...init.core.evaluation,
        write_out_steps,
      },
    },
  };
};

let writes_out = (model, settings) =>
  Mode.Model.settings_for(model, settings).core.evaluation.write_out_steps;

let flag_turns_mode_on = () => {
  check(
    bool,
    "the flag on",
    true,
    writes_out(model(~write_out_steps=true), settings(false)),
  );
  check(
    bool,
    "the flag off, the setting on",
    true,
    writes_out(model(~write_out_steps=false), settings(true)),
  );
  check(
    bool,
    "both off",
    false,
    writes_out(model(~write_out_steps=false), settings(false)),
  );
};

let only_instructor_toggles = () => {
  let toggled = (~instructor_mode) =>
    Mode.Update.update(
      ~settings=settings(~instructor_mode, false),
      Instructor(ToggleWriteOutSteps),
      model(~write_out_steps=false),
    ).
      model.
      write_out_steps;
  check(bool, "a student", false, toggled(~instructor_mode=false));
  check(bool, "an instructor", true, toggled(~instructor_mode=true));
};

/* A saved exercise, as sexp and as JSON, with the flag or without the
 * field. */
let round_trip = () => {
  let empty = Haz3lcore.PersistentZipper.persist(Haz3lcore.Zipper.init());
  let saved: TheoremExercise.persistent_spec = {
    id: Haz3lcore.Id.mk(),
    title: "t",
    module_name: "T",
    prompt: "",
    max_points: 10,
    prelude: empty,
    lemmas: empty,
    theorem: empty,
    write_out_steps: true,
  };
  let sexp = TheoremExercise.sexp_of_persistent_spec(saved);
  let json = TheoremExercise.yojson_of_persistent_spec(saved);
  let loaded = (spec: TheoremExercise.persistent_spec) =>
    TheoremExercise.of_persistent(spec).write_out_steps;
  check(
    bool,
    "sexp",
    true,
    loaded(TheoremExercise.persistent_spec_of_sexp(sexp)),
  );
  check(
    bool,
    "JSON",
    true,
    loaded(TheoremExercise.persistent_spec_of_yojson(json)),
  );
  let without_sexp =
    switch (sexp) {
    | List(fields) =>
      Sexplib.Sexp.List(
        List.filter(
          ~f=
            (field: Sexplib.Sexp.t) =>
              switch (field) {
              | List([Atom("write_out_steps"), ..._]) => false
              | _ => true
              },
          fields,
        ),
      )
    | Atom(_) => Alcotest.fail("not a record")
    };
  let without_json =
    switch (json) {
    | `Assoc(fields) =>
      `Assoc(
        List.filter(
          ~f=((name, _)) => !String.equal(name, "write_out_steps"),
          fields,
        ),
      )
    | _ => Alcotest.fail("not a record")
    };
  check(
    bool,
    "sexp without the field",
    false,
    loaded(TheoremExercise.persistent_spec_of_sexp(without_sexp)),
  );
  check(
    bool,
    "JSON without the field",
    false,
    loaded(TheoremExercise.persistent_spec_of_yojson(without_json)),
  );
};

let tests = (
  "TheoremExercise",
  [
    test_case(
      "the exercise's flag turns writing out steps on, and can't turn it off",
      `Quick,
      flag_turns_mode_on,
    ),
    test_case(
      "only an instructor toggles writing out steps",
      `Quick,
      only_instructor_toggles,
    ),
    test_case(
      "an exercise loads with and without its write-out flag",
      `Quick,
      round_trip,
    ),
  ],
);
