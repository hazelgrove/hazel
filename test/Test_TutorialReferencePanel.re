open Alcotest;
open Web;

let lesson = title =>
  List.find_exn(TutorialSettings.lessons, ~f=(s: Tutorial.spec) =>
    String.equal(s.title, title)
  );

let panel = title =>
  switch (TutorialReferencePanel.of_lesson(lesson(title))) {
  | Some(context) => context
  | None => fail("Missing reference panel for " ++ title)
  };

let tests = (
  "TutorialReferencePanel",
  [
    test_case(
      "controls without prose still expose the panel",
      `Quick,
      () => {
        let context = panel("Probes / Reading Bigger Values");
        check(
          option(string),
          "no synthetic reference text",
          None,
          context.reference,
        );
        check(
          bool,
          "probe controls available",
          true,
          Option.is_some(context.probe_config),
        );
      },
    ),
    test_case("prose-only lessons need no probe controls", `Quick, () => {
      ["Basics / Holes", "Probes / Intro"]
      |> List.iter(~f=title => {
           let context = panel(title);
           check(
             bool,
             title ++ " reference prose available",
             true,
             Option.is_some(context.reference),
           );
           check(
             bool,
             title ++ " no probe controls yet",
             true,
             Option.is_none(context.probe_config),
           );
         })
    }),
    test_case(
      "print lesson keeps reference and console together",
      `Quick,
      () => {
        let context = panel("Probes / Print Statements");
        check(
          bool,
          "reference prose available",
          true,
          Option.is_some(context.reference),
        );
        switch (context.probe_config) {
        | Some(config) =>
          check(
            bool,
            "console available",
            true,
            ProbeControls.mem(config.flags, Console),
          )
        | None => fail("Print lesson has no console configuration")
        };
      },
    ),
    test_case("lesson without panel content exposes no tab", `Quick, () => {
      check(
        bool,
        "no reference tab",
        true,
        Option.is_none(
          TutorialReferencePanel.of_lesson(lesson("Probes / Tasks Ahead")),
        ),
      )
    }),
  ],
);
