/*
 * GenTutorial: Generate Tutorial.spec ML files from tutorial text files.
 * =====================================================================
 *
 * This is the Tutorial-mode counterpart to GenSlides. Where GenSlides emits
 * (title, PersistentSegment.t) Documentation slides, GenTutorial emits
 * Tutorial.spec records that render as gated Tutorial-mode lessons (prompt
 * panel + editor + hidden tests).
 *
 * USAGE:
 *   ./hazel gen-tutorial         # Generate Tutorial.spec files from text
 *   ./hazel gen-tutorial-clean   # Remove generated files, restore empty stub
 *   dune build                   # Rebuild after generating
 *
 * INPUT FORMAT (hazel-programs/tutorial/<NN-name>.hz):
 *   A plain text file. Optional section markers (a line that is exactly
 *   `@prompt`, `@code`, or `@test`) split the file into:
 *     @prompt  -> markdown shown in the instructions panel
 *     @code    -> Hazel source loaded into the editor (your_impl)
 *     @test    -> Hazel `test ... end` used as the (currently non-gating)
 *                 hidden test; defaults to `test true end` if omitted
 *   With NO markers, the ENTIRE file is treated as @code (so the existing
 *   probe-study tutorial .hz files convert verbatim, instructions inline as
 *   comments). This is the v1 "bring the editor content over" path.
 *
 * OUTPUT (src/web/exercises/examples/):
 *   - TuGen_<Name>.ml          one Tutorial.spec per input file
 *   - TutorialGenerated.ml     aggregation: `let all : Tutorial.spec list`
 *   These live INSIDE the `web` library (examples/ is under
 *   `(include_subdirs unqualified)`), so they can reference Tutorial.spec.
 *
 * WIRING (one-time, already done):
 *   src/web/exercises/settings/TutorialSettings_base.re appends
 *   `@ TutorialGenerated.all` to the hand-written `lessons` list, so
 *   generated slides show up after the onboarding lessons.
 *
 * NOTE: gating. Each generated slide gets a placeholder `test true end`
 * hidden test, which trivially passes (the slide shows ✔ immediately). Add a
 * real `@test` section to gate a slide on a meaningful condition.
 */

let input_dir = "hazel-programs/tutorial";
let output_dir = "src/web/exercises/examples";
let module_prefix = "TuGen_";
let aggregation_module = "TutorialGenerated.ml";

/* Strip common leading indentation from @code before parsing (matches
   GenSlides; the tutorial .hz files are authored at column 0). */
let strip_indentation = true;

let default_prompt = "Work through the inline instructions in the editor below.";

/* Write a string to a file */
let write_file = (path: string, content: string): unit =>
  Core.Out_channel.write_all(path, ~data=content);

/* Pick a {tag|...|tag} quoted-string delimiter not present in `content`. */
let rec pick_tag = (content: string, tag: string): string =>
  if (Core.String.is_substring(content, ~substring="|" ++ tag ++ "}")) {
    pick_tag(content, tag ++ "z");
  } else {
    tag;
  };

/* Emit an OCaml double-quoted string literal for simple values. */
let ocaml_string = (s: string): string => "\"" ++ String.escaped(s) ++ "\"";

type sections = {
  prompt: string,
  code: string,
  test: string,
};

/* Split input on `@prompt` / `@code` / `@test` marker lines. Default
   section is `code`, so a file with no markers is entirely code. */
let parse_sections = (content: string): sections => {
  let lines = String.split_on_char('\n', content);
  let (prompt, code, test, _) =
    List.fold_left(
      ((p, c, t, cur), line) =>
        switch (String.trim(line)) {
        | "@prompt" => (p, c, t, `Prompt)
        | "@code" => (p, c, t, `Code)
        | "@test" => (p, c, t, `Test)
        | _ =>
          switch (cur) {
          | `Prompt => (p ++ line ++ "\n", c, t, cur)
          | `Code => (p, c ++ line ++ "\n", t, cur)
          | `Test => (p, c, t ++ line ++ "\n", cur)
          }
        },
      ("", "", "", `Code),
      lines,
    );
  {prompt: String.trim(prompt), code, test: String.trim(test)};
};

/* "01-fundamentals.hz" -> "TuGen_01Fundamentals" */
let module_name_of = (rel: string): string => {
  let base = Filename.chop_suffix(rel, ".hz");
  let camel =
    String.split_on_char('-', base)
    |> List.map(String.capitalize_ascii)
    |> String.concat("");
  module_prefix ++ camel;
};

/* "01-fundamentals.hz" -> "01 Fundamentals" */
let title_of = (rel: string): string =>
  Filename.chop_suffix(rel, ".hz")
  |> String.split_on_char('-')
  |> List.map(String.capitalize_ascii)
  |> String.concat(" ");

/* Deterministic, valid UUID per index (stable localStorage keys across
   regenerations). Uses a high first group to avoid colliding with the
   hand-written specs' "a..." ids. */
let id_string = (i: int): string =>
  Printf.sprintf("%08x-7507-4000-8000-000000000000", 0x70000000 + i);

let find_hz_files = (): list(string) =>
  try(
    Sys.readdir(input_dir)
    |> Array.to_list
    |> List.filter(e => Filename.check_suffix(e, ".hz"))
    |> List.sort(String.compare)
  ) {
  | Sys_error(msg) =>
    prerr_endline("Warning: " ++ msg);
    [];
  };

let generate_ml_file = (i: int, rel_path: string): option(string) => {
  let input_path = input_dir ++ "/" ++ rel_path;
  let module_name = module_name_of(rel_path);
  let title = title_of(rel_path);
  let output_path = output_dir ++ "/" ++ module_name ++ ".ml";
  try({
    let raw = Core.In_channel.read_all(input_path);
    let {prompt, code, test} = parse_sections(raw);
    let code = strip_indentation ? Util.StringUtil.trim_leading(code) : code;
    let code = String.trim(code);
    /* Validate the editor code parses; warn but still emit (the generated
       Option.get would otherwise raise at load time). */
    switch (Haz3lcore.Parser.to_zipper(~root=Exp, code)) {
    | None => prerr_endline("WARNING: @code failed to parse in " ++ rel_path)
    | Some(_) => ()
    };
    let test = test == "" ? "test true end" : test;
    let prompt = prompt == "" ? default_prompt : prompt;
    let code_tag = pick_tag(code, "hz");
    let test_tag = pick_tag(test, "hz");
    let prompt_tag = pick_tag(prompt, "md");
    let ml =
      "(* Auto-generated by: ./hazel gen-tutorial -- DO NOT EDIT *)\n"
      ++ "(* Source: "
      ++ input_path
      ++ " *)\n\n"
      ++ "let exercise : Tutorial.spec = {\n"
      ++ "  id = Option.get (Haz3lcore.Id.of_string "
      ++ ocaml_string(id_string(i))
      ++ ");\n"
      ++ "  title = "
      ++ ocaml_string(title)
      ++ ";\n"
      ++ "  version = 1;\n"
      ++ "  module_name = "
      ++ ocaml_string(module_name)
      ++ ";\n"
      ++ "  prompt = {"
      ++ prompt_tag
      ++ "|"
      ++ prompt
      ++ "|"
      ++ prompt_tag
      ++ "};\n"
      ++ "  display_hint = \"\";\n"
      ++ "  task_reference = \"\";\n"
      ++ "  your_impl =\n    Option.get (Haz3lcore.Parser.to_zipper ~root:Exp {"
      ++ code_tag
      ++ "|"
      ++ code
      ++ "|"
      ++ code_tag
      ++ "});\n"
      ++ "  hidden_tests =\n    {\n      tests =\n        Option.get (Haz3lcore.Parser.to_zipper ~root:Exp {"
      ++ test_tag
      ++ "|"
      ++ test
      ++ "|"
      ++ test_tag
      ++ "});\n      hints = [];\n    };\n"
      ++ "  wrapper = false;\n"
      ++ "  show_report = false;\n"
      ++ "}\n";
    write_file(output_path, ml);
    print_endline("Generated: " ++ module_name ++ ".ml");
    Some(module_name);
  }) {
  | exn =>
    prerr_endline(
      "Error processing " ++ rel_path ++ ": " ++ Printexc.to_string(exn),
    );
    None;
  };
};

let generate_aggregation = (modules: list(string)): unit => {
  let refs =
    modules
    |> List.map(m => "  " ++ m ++ ".exercise;")
    |> String.concat("\n");
  let content =
    "(* Auto-generated by: ./hazel gen-tutorial *)\n"
    ++ "(* To remove: ./hazel gen-tutorial-clean *)\n\n"
    ++ "let all : Tutorial.spec list = [\n"
    ++ refs
    ++ "\n]\n";
  write_file(output_dir ++ "/" ++ aggregation_module, content);
  print_endline("Generated: " ++ aggregation_module);
};

let generate = (): unit => {
  print_endline("Generating tutorial slides from: " ++ input_dir);
  let files = find_hz_files();
  print_endline("Found " ++ string_of_int(List.length(files)) ++ " .hz files\n");
  let modules =
    List.mapi((i, f) => generate_ml_file(i, f), files)
    |> List.filter_map(x => x);
  generate_aggregation(modules);
  print_endline(
    "\nDone! Generated "
    ++ string_of_int(List.length(modules))
    ++ " tutorial slides.",
  );
};

let clean = (): unit => {
  print_endline("Cleaning generated tutorial slides from: " ++ output_dir);
  (
    try(
      Sys.readdir(output_dir)
      |> Array.to_list
      |> List.iter(entry =>
           if (Filename.check_suffix(entry, ".ml")
               && String.length(entry) >= String.length(module_prefix)
               && String.sub(entry, 0, String.length(module_prefix))
               == module_prefix) {
             Sys.remove(output_dir ++ "/" ++ entry);
             print_endline("Removed: " ++ entry);
           }
         )
    ) {
    | Sys_error(msg) => prerr_endline("Warning: " ++ msg)
    }
  );
  write_file(
    output_dir ++ "/" ++ aggregation_module,
    "(* Stub - run ./hazel gen-tutorial to populate *)\n\n"
    ++ "let all : Tutorial.spec list = []\n",
  );
  print_endline("Done! Restored to empty stub.");
};
