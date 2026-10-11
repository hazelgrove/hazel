open Alcotest;
open Haz3lcore;
open Web;

let lesson = title =>
  List.find_exn(TutorialSettings.lessons, ~f=(s: Tutorial.spec) =>
    String.equal(s.title, title)
  );

let with_settings = f => {
  let saved = ProbeProj.Settings.s^;
  let autoprobe = ref(AutoProbe.Caret);
  let set_autoprobe = mode => autoprobe := mode;
  let enter = spec =>
    TutorialSlideInit.maybe_apply_on_change(
      ~autoprobe=autoprobe^,
      ~set_autoprobe,
      spec,
    );
  ProbeProj.Settings.go(SetWindow(Many));
  ProbeProj.Settings.go(SetSampleBase(Calls));
  Exn.protect(
    ~finally=
      () => {
        enter(None);
        ProbeProj.Settings.s := saved;
      },
    ~f=() => f(autoprobe, enter),
  );
};

let check_settings = (label, autoprobe, expected_auto, window, colors) => {
  check(
    bool,
    label ++ " auto-probe",
    true,
    AutoProbe.equal(autoprobe^, expected_auto),
  );
  check(
    bool,
    label ++ " samples",
    true,
    Poly.equal(ProbeProj.Settings.s^.window, window),
  );
  check(
    bool,
    label ++ " colors",
    true,
    Poly.equal(ProbeProj.Settings.s^.sample_base, colors),
  );
};

let tests = (
  "TutorialProbeSettings",
  [
    test_case("other folders leave settings unchanged", `Quick, () =>
      with_settings((autoprobe, enter) => {
        TutorialSettings.lessons
        |> List.filter(~f=s => !Tutorial.is_probes_lesson(s))
        |> List.iter(~f=(s: Tutorial.spec) => {
             enter(Some(s));
             check_settings(s.title, autoprobe, Caret, Many, Calls);
           })
      })
    ),
    test_case(
      "manual choices survive updates, but not lesson re-entry", `Quick, () =>
      with_settings((autoprobe, enter) => {
        let arithmetic = lesson("Probes / Arithmetic and Holes");
        enter(Some(arithmetic));
        check_settings("initial", autoprobe, Off, Single, Simple);
        autoprobe := All;
        ProbeProj.Settings.go(SetWindow(Many));
        ProbeProj.Settings.go(SetSampleBase(Hybrid));
        enter(Some(arithmetic));
        check_settings("same lesson", autoprobe, All, Many, Hybrid);
        enter(Some(lesson("Probes / Folding over a List")));
        check_settings("fold", autoprobe, All, Many, Simple);
        enter(Some(arithmetic));
        check_settings("re-enter", autoprobe, Off, Single, Simple);
      })
    ),
    test_case("leaving Probes restores the original settings", `Quick, () =>
      with_settings((autoprobe, enter) => {
        enter(Some(lesson("Probes / Arithmetic and Holes")));
        enter(Some(lesson("Probes / Bonus - Sample Colors")));
        check_settings("colors lesson", autoprobe, All, Many, Hybrid);
        enter(Some(lesson("Basics / Holes")));
        check_settings("back to Basics", autoprobe, Caret, Many, Calls);
        /* A second visit saves the user's new choices, not the first visit's. */
        autoprobe := Off;
        ProbeProj.Settings.go(SetWindow(Single));
        enter(Some(lesson("Probes / Folding over a List")));
        enter(None);
        check_settings("outside tutorials", autoprobe, Off, Single, Calls);
      })
    ),
    test_case("page startup initializes a saved Probes lesson", `Quick, () =>
      with_settings((autoprobe, _enter) => {
        let globals = Globals.Model.init();
        let globals = {
          ...globals,
          settings: {
            ...globals.settings,
            autoprobe_mode: autoprobe^,
          },
        };
        let editors: Editors.Model.t =
          Tutorial({
            current: 0,
            exercises: [
              TutorialMode.Model.of_spec(
                ~settings=globals.settings.core,
                ~instructor_mode=false,
                lesson("Probes / Arithmetic and Holes"),
              ),
            ],
          });
        let model: Page.Model.t = {
          globals,
          editors,
          explain_this: ExplainThisModel.init,
          selection: Editors.Selection.default_selection(editors),
        };
        let requested = ref([]);
        let _ =
          Page.Update.update(
            ~import_log=_ => (),
            ~get_log_and=_ => (),
            ~schedule_action=a => requested := [a, ...requested^],
            Start,
            model,
          );
        check(
          bool,
          "startup schedules auto-probe Off",
          true,
          List.exists(
            ~f=
              a =>
                switch (a) {
                | Page.Update.Globals(Set(SetAutoprobe(Off))) => true
                | _ => false
                },
            requested^,
          ),
        );
        check(
          bool,
          "startup resets samples",
          true,
          Poly.equal(ProbeProj.Settings.s^.window, Single),
        );
        check(
          bool,
          "startup resets colors",
          true,
          Poly.equal(ProbeProj.Settings.s^.sample_base, Simple),
        );
      })
    ),
  ],
);
