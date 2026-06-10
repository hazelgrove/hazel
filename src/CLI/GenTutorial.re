/*
 * GenTutorial: Generate Tutorial.spec ML files from tutorial text files.
 * =====================================================================
 *
 * Tutorial-mode counterpart to GenSlides. Emits Tutorial.spec records that
 * render as gated Tutorial-mode lessons (prompt panel + editor + hidden
 * tests). The inverse direction (existing spec -> text) lives in
 * TutorialDecode.re (`./hazel tutorial-decode`).
 *
 * USAGE:
 *   ./hazel gen-tutorial         # Generate Tutorial.spec files from text
 *   ./hazel gen-tutorial-clean   # Remove generated files, restore empty stub
 *   dune build
 *
 * INPUT FORMAT (hazel-programs/tutorial/<NN-name>.hz):
 *   Plain text split by marker lines that are *exactly*:
 *     @prompt     -> markdown shown in the instructions panel
 *     @code       -> editor contents (your_impl)  [REQUIRED in practice]
 *     @test       -> hidden test; defaults to `test true end`
 *     @hint       -> display_hint (short string)
 *     @reference  -> task_reference markdown
 *     @hints      -> one hint string per non-empty line (hidden_tests.hints)
 *     @flags      -> whitespace-separated tokens: `wrapper`, `show_report`,
 *                    `version=N`, `id=<uuid>` (carried through round-trips)
 *   With NO markers, the entire file is treated as @code.
 *
 *   Inside @code and @test, a line that is exactly `{{include:rel/path}}`
 *   (path relative to the repo root) is replaced with that file's contents.
 *   Task slides use this to reference a canonical program (e.g. under
 *   hazel-programs/study/tasks-draft) instead of embedding a drifting copy.
 *
 *   Code/test are parsed with TextRoundtrip.of_text, so the `¿` implicit-hole
 *   marker and `^^probe(...)` projector syntax produced by `tutorial-decode`
 *   round-trip correctly.
 *
 * OUTPUT (src/web/exercises/examples/): TuGen_<Name>.ml + TutorialGenerated.ml
 * (`let all : Tutorial.spec list`), appended to `lessons` in
 * TutorialSettings_base.re.
 */

/* Conjoined study tutorial: the single ordered sequence lives in the
   conjoined/ subdir. The sibling basics/, probes/, archive/ dirs are kept as
   reference sources and are intentionally not generated. To regenerate the old
   full set instead, point this back at "hazel-programs/tutorial". */
let input_dir = "hazel-programs/tutorial/conjoined";
let output_dir = "src/web/exercises/examples";
let module_prefix = "TuGen_";
let aggregation_module = "TutorialGenerated.ml";
let strip_indentation = true;
let default_prompt = "Work through the inline instructions in the editor below.";

let write_file = (path: string, content: string): unit =>
  Core.Out_channel.write_all(path, ~data=content);

/* Pick a {tag|...|tag} delimiter not present in `content`. */
let rec pick_tag = (content: string, tag: string): string =>
  if (Core.String.is_substring(content, ~substring="|" ++ tag ++ "}")) {
    pick_tag(content, tag ++ "z");
  } else {
    tag;
  };

let ocaml_string = (s: string): string => "\"" ++ String.escaped(s) ++ "\"";

/* OCaml {tag|...|tag} quoted-string literal with a safe delimiter. */
let quoted = (s: string): string => {
  let t = pick_tag(s, "x");
  "{" ++ t ++ "|" ++ s ++ "|" ++ t ++ "}";
};

type sections = {
  prompt: string,
  code: string,
  test: string,
  hint: string,
  reference: string,
  hints: list(string),
  wrapper: bool,
  show_report: bool,
  version: int,
  id: option(string),
};

let empty_sections = {
  prompt: "",
  code: "",
  test: "",
  hint: "",
  reference: "",
  hints: [],
  wrapper: false,
  show_report: false,
  version: 1,
  id: None,
};

let parse_flags = (s: sections, body: string): sections => {
  let toks =
    String.split_on_char('\n', body)
    |> List.concat_map(String.split_on_char(' '))
    |> List.map(String.trim)
    |> List.filter(t => t != "");
  List.fold_left(
    (acc, tok) =>
      switch (tok) {
      | "wrapper" => {
          ...acc,
          wrapper: true,
        }
      | "show_report" => {
          ...acc,
          show_report: true,
        }
      | _ when String.length(tok) > 8 && String.sub(tok, 0, 8) == "version=" => {
          ...acc,
          version:
            try(int_of_string(String.sub(tok, 8, String.length(tok) - 8))) {
            | _ => acc.version
            },
        }
      | _ when String.length(tok) > 3 && String.sub(tok, 0, 3) == "id=" => {
          ...acc,
          id: Some(String.sub(tok, 3, String.length(tok) - 3)),
        }
      | _ => acc
      },
    s,
    toks,
  );
};

/* Split input on `@prompt`/`@code`/`@test`/`@hint`/`@reference`/`@hints`/`@flags`
   marker lines. Default section is `code`. */
let parse_sections = (content: string): sections => {
  let lines = String.split_on_char('\n', content);
  /* accumulate raw section bodies keyed by name */
  let (acc, _cur) =
    List.fold_left(
      ((acc, cur), line) =>
        switch (String.trim(line)) {
        | "@prompt" => (acc, `Prompt)
        | "@code" => (acc, `Code)
        | "@test" => (acc, `Test)
        | "@hint" => (acc, `Hint)
        | "@reference" => (acc, `Reference)
        | "@hints" => (acc, `Hints)
        | "@flags" => (acc, `Flags)
        | _ =>
          let key =
            switch (cur) {
            | `Prompt => "prompt"
            | `Code => "code"
            | `Test => "test"
            | `Hint => "hint"
            | `Reference => "reference"
            | `Hints => "hints"
            | `Flags => "flags"
            };
          let prev =
            try(List.assoc(key, acc)) {
            | Not_found => ""
            };
          (
            [(key, prev ++ line ++ "\n"), ...List.remove_assoc(key, acc)],
            cur,
          );
        },
      ([], `Code),
      lines,
    );
  let get = k =>
    try(List.assoc(k, acc)) {
    | Not_found => ""
    };
  let s = {
    ...empty_sections,
    prompt: String.trim(get("prompt")),
    code: get("code"),
    test: String.trim(get("test")),
    hint: String.trim(get("hint")),
    reference: String.trim(get("reference")),
    hints:
      String.split_on_char('\n', get("hints"))
      |> List.map(String.trim)
      |> List.filter(h => h != ""),
  };
  parse_flags(s, get("flags"));
};

/* Replace `{{include:rel/path}}` lines with the referenced file's contents
   (path resolved from the repo root, i.e. the CLI's working directory). A
   missing file raises, surfacing as the per-slide error in generate_ml_file. */
let expand_includes = (body: string): string => {
  let pre = "{{include:";
  let suf = "}}";
  String.split_on_char('\n', body)
  |> List.map(line => {
       let t = String.trim(line);
       let is_include =
         String.length(t) > String.length(pre)
         + String.length(suf)
         && String.sub(t, 0, String.length(pre)) == pre
         && String.sub(t, String.length(t) - String.length(suf), 2) == suf;
       if (is_include) {
         let path =
           String.sub(
             t,
             String.length(pre),
             String.length(t) - String.length(pre) - String.length(suf),
           );
         String.trim(Core.In_channel.read_all(String.trim(path)));
       } else {
         line;
       };
     })
  |> String.concat("\n");
};

let module_name_of = (rel: string): string => {
  let base = Filename.chop_suffix(rel, ".hz");
  let camel =
    String.split_on_char('/', base)
    |> List.concat_map(String.split_on_char('-'))
    |> List.map(String.capitalize_ascii)
    |> String.concat("");
  module_prefix ++ camel;
};

let cap_words = (s: string): string =>
  String.split_on_char('-', s)
  |> List.filter(w => w != "")
  |> List.map(String.capitalize_ascii)
  |> String.concat(" ");

let is_digits = (s: string): bool =>
  s != "" && String.for_all(c => c >= '0' && c <= '9', s);

/* True when the token begins with a digit, e.g. "13" or the inserted "13b".
   Used so an inserted slide like "13b - Watch It Build" keeps the " - "
   separator that purely-numeric prefixes get. */
let starts_with_digit = (s: string): bool =>
  String.length(s) > 0
  && {
    let c = s.[0];
    c >= '0' && c <= '9';
  };

/* "basics/01-holes" -> "Basics / 01 - Holes"; a leading numeric token in the
   filename is separated from the title words by " - ". */
let title_of = (rel: string): string => {
  let segs = String.split_on_char('/', Filename.chop_suffix(rel, ".hz"));
  switch (List.rev(segs)) {
  | [] => ""
  | [last, ...rev_dirs] =>
    let file_title =
      switch (String.split_on_char('-', last)) {
      | [num, ...rest]
          when starts_with_digit(num) && List.exists(w => w != "", rest) =>
        num
        ++ " - "
        ++ (
          rest
          |> List.filter(w => w != "")
          |> List.map(String.capitalize_ascii)
          |> String.concat(" ")
        )
      | _ => cap_words(last)
      };
    let prefix =
      List.rev(rev_dirs) |> List.map(cap_words) |> String.concat(" / ");
    prefix == "" ? file_title : prefix ++ " / " ++ file_title;
  };
};

let id_string = (i: int): string =>
  Printf.sprintf("%08x-7507-4000-8000-000000000000", 0x70000000 + i);

let rec find_hz_files = (base: string, rel: string): list(string) => {
  let full = rel == "" ? base : base ++ "/" ++ rel;
  try(
    Sys.readdir(full)
    |> Array.to_list
    |> List.concat_map(entry => {
         let entry_rel = rel == "" ? entry : rel ++ "/" ++ entry;
         if (Sys.is_directory(full ++ "/" ++ entry)) {
           find_hz_files(base, entry_rel);
         } else if (Filename.check_suffix(entry, ".hz")) {
           [entry_rel];
         } else {
           [];
         };
       })
  ) {
  | Sys_error(msg) =>
    prerr_endline("Warning: " ++ msg);
    [];
  };
};

let generate_ml_file = (i: int, rel_path: string): option(string) => {
  let input_path = input_dir ++ "/" ++ rel_path;
  let module_name = module_name_of(rel_path);
  let title = title_of(rel_path);
  let output_path = output_dir ++ "/" ++ module_name ++ ".ml";
  try({
    let raw = Core.In_channel.read_all(input_path);
    let s = parse_sections(raw);
    let code =
      strip_indentation ? Util.StringUtil.trim_leading(s.code) : s.code;
    let code = String.trim(expand_includes(code));
    switch (Haz3lcore.TextRoundtrip.of_text(~root=Exp, code)) {
    | None => prerr_endline("WARNING: @code failed to parse in " ++ rel_path)
    | Some(_) => ()
    };
    let test = s.test == "" ? "test true end" : expand_includes(s.test);
    let prompt = s.prompt == "" ? default_prompt : s.prompt;
    let id = Option.value(s.id, ~default=id_string(i));
    let hints_ml =
      "["
      ++ (s.hints |> List.map(ocaml_string) |> String.concat("; "))
      ++ "]";
    let ml =
      "(* Auto-generated by: ./hazel gen-tutorial -- DO NOT EDIT *)\n"
      ++ "(* Source: "
      ++ input_path
      ++ " *)\n\n"
      ++ "let exercise : Tutorial.spec = {\n"
      ++ "  id = Option.get (Haz3lcore.Id.of_string "
      ++ ocaml_string(id)
      ++ ");\n"
      ++ "  title = "
      ++ ocaml_string(title)
      ++ ";\n"
      ++ "  version = "
      ++ string_of_int(s.version)
      ++ ";\n"
      ++ "  module_name = "
      ++ ocaml_string(module_name)
      ++ ";\n"
      ++ "  prompt = "
      ++ quoted(prompt)
      ++ ";\n"
      ++ "  display_hint = "
      ++ quoted(s.hint)
      ++ ";\n"
      ++ "  task_reference = "
      ++ quoted(s.reference)
      ++ ";\n"
      ++ "  your_impl =\n    Haz3lcore.Zipper.caret_to_start (Option.get (Haz3lcore.TextRoundtrip.of_text ~root:Exp "
      ++ quoted(code)
      ++ "));\n"
      ++ "  hidden_tests =\n    {\n      tests =\n        Option.get (Haz3lcore.TextRoundtrip.of_text ~root:Exp "
      ++ quoted(test)
      ++ ");\n      hints = "
      ++ hints_ml
      ++ ";\n    };\n"
      ++ "  wrapper = "
      ++ string_of_bool(s.wrapper)
      ++ ";\n"
      ++ "  show_report = "
      ++ string_of_bool(s.show_report)
      ++ ";\n"
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
    modules |> List.map(m => "  " ++ m ++ ".exercise;") |> String.concat("\n");
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
  let files = find_hz_files(input_dir, "") |> List.sort(String.compare);
  print_endline(
    "Found " ++ string_of_int(List.length(files)) ++ " .hz files\n",
  );
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
  };
  write_file(
    output_dir ++ "/" ++ aggregation_module,
    "(* Stub - run ./hazel gen-tutorial to populate *)\n\n"
    ++ "let all : Tutorial.spec list = []\n",
  );
  print_endline("Done! Restored to empty stub.");
};
