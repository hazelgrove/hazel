/*
 * TutorialDecode: the inverse of GenTutorial. Reads the in-memory
 * Tutorial.spec list (Web.TutorialSettings.lessons) and emits the
 * @prompt/@code/@test/... text format that gen-tutorial consumes, using
 * MarkerParse to render the editor zippers as text.
 *
 *   ./hazel tutorial-decode            # write all hand-written lessons to
 *                                      #   hazel-programs/tutorial/imported/
 *   ./hazel tutorial-decode SUBSTR     # print lessons whose title matches
 *                                      #   SUBSTR to stdout (for inspection)
 *   ./hazel tutorial-verify            # round-trip-check every slide
 *
 * VERIFICATION: `tutorial-verify` checks the property slide-cli guarantees
 * for parser-originated programs: to_text(z) is a fixed point of
 * (of_text >> to_text). If a slide's impl/tests text is stable across a
 * decode->encode->decode cycle, the generated slide reproduces the original's
 * content (IDs aside). It reports per-slide OK/MISMATCH and a summary.
 */

open Haz3lcore;

/* (text, text-after-one-roundtrip); equal => the content is round-trip stable */
let roundtrip = (z: Zipper.t): (string, string) => {
  let t1 = MarkerParse.to_text(z);
  let t2 =
    switch (MarkerParse.of_text(~root=Exp, t1)) {
    | None => "<<PARSE FAILED>>"
    | Some(z2) => MarkerParse.to_text(z2)
    };
  (t1, t2);
};

let kv = (name: string, body: string): string =>
  "@" ++ name ++ "\n" ++ body ++ "\n\n";

let decode_spec = (spec: Web.Tutorial.spec): string =>
  Web.Tutorial.(
    let code = MarkerParse.to_text(spec.your_impl);
    let test = MarkerParse.to_text(spec.hidden_tests.tests);
    let flags =
      (spec.wrapper ? ["wrapper"] : [])
      @ (spec.show_report ? ["show_report"] : [])
      @ ["version=" ++ string_of_int(spec.version)]
      @ ["id=" ++ Id.to_string(spec.id)];
    kv("flags", String.concat(" ", flags))
    ++ kv("prompt", spec.prompt)
    ++ (spec.display_hint == "" ? "" : kv("hint", spec.display_hint))
    ++ (
      spec.task_reference == "" ? "" : kv("reference", spec.task_reference)
    )
    ++ (
      spec.hidden_tests.hints == []
        ? "" : kv("hints", String.concat("\n", spec.hidden_tests.hints))
    )
    ++ kv("code", String.trim(code))
    ++ "@test\n"
    ++ String.trim(test)
    ++ "\n"
  );

let is_generated = (spec: Web.Tutorial.spec): bool =>
  Web.Tutorial.(
    String.length(spec.module_name) >= 6
    && String.sub(spec.module_name, 0, 6) == "TuGen_"
  );

let title_of = (spec: Web.Tutorial.spec): string => Web.Tutorial.(spec.title);

let kebab = (s: string): string =>
  String.lowercase_ascii(s)
  |> String.map(c => c >= 'a' && c <= 'z' || c >= '0' && c <= '9' ? c : '-');

let rec ensure_dir = (path: string): unit =>
  if (!Sys.file_exists(path)) {
    let parent = Filename.dirname(path);
    if (parent != path && parent != ".") {
      ensure_dir(parent);
    };
    try(Unix.mkdir(path, 0o755)) {
    | _ => ()
    };
  };

let print_matching = (substr: string): unit =>
  List.iter(
    spec =>
      if (substr == ""
          || Core.String.is_substring(title_of(spec), ~substring=substr)) {
        print_endline("=== " ++ title_of(spec) ++ " ===");
        print_string(decode_spec(spec));
        print_endline("");
      },
    Web.TutorialSettings.lessons,
  );

let write_all = (dir: string): unit => {
  ensure_dir(dir);
  let specs =
    Web.TutorialSettings.lessons |> List.filter(s => !is_generated(s));
  List.iteri(
    (i, spec) => {
      let name =
        Printf.sprintf("%02d-%s.hzt", i + 1, kebab(title_of(spec)));
      Core.Out_channel.write_all(
        dir ++ "/" ++ name,
        ~data=decode_spec(spec),
      );
      print_endline("Wrote: " ++ dir ++ "/" ++ name);
    },
    specs,
  );
  print_endline(
    "\nWrote "
    ++ string_of_int(List.length(specs))
    ++ " hand-written lessons to "
    ++ dir,
  );
};

let decode = (substr: option(string)): unit =>
  switch (substr) {
  | Some(s) => print_matching(s)
  | None => write_all("hazel-programs/tutorial-imported")
  };

let show_diff = (label: string, a: string, b: string): unit =>
  Printf.printf(
    "  --- %s before roundtrip ---\n%s\n  --- %s after roundtrip ---\n%s\n",
    label,
    a,
    label,
    b,
  );

let verify = (verbose: bool): unit => {
  let (pass, fail) =
    List.fold_left(
      ((pass, fail), spec) =>
        Web.Tutorial.(
          {
            let (i1, i2) = roundtrip(spec.your_impl);
            let (t1, t2) = roundtrip(spec.hidden_tests.tests);
            let impl_ok = i1 == i2;
            let test_ok = t1 == t2;
            Printf.printf(
              "%-34s impl:%-9s tests:%s\n",
              spec.title,
              impl_ok ? "OK" : "MISMATCH",
              test_ok ? "OK" : "MISMATCH",
            );
            if (verbose && !impl_ok) {
              show_diff("impl", i1, i2);
            };
            if (verbose && !test_ok) {
              show_diff("tests", t1, t2);
            };
            impl_ok && test_ok ? (pass + 1, fail) : (pass, fail + 1);
          }
        ),
      (0, 0),
      Web.TutorialSettings.lessons,
    );
  Printf.printf(
    "\n%d/%d slides round-trip cleanly (%d mismatched)\n",
    pass,
    pass + fail,
    fail,
  );
};
