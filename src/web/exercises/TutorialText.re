/*
 * TutorialText: build Tutorial.spec records from the .hzt lesson sources
 * embedded by the tutorialslides library. This replaces the old
 * `./hazel gen-tutorial` codegen step: the .hzt files are compiled in as
 * raw text (ppx_blob) and parsed here at startup, so editing a slide is
 * just editing its .hzt and rebuilding.
 *
 * The .hzt marker format (see hazel-programs/tutorial/README.md):
 * @prompt/@code/@test/@hint/@reference/@hints/@flags section lines; the
 * default (markerless) section is @code. The inverse direction
 * (spec -> text) lives in src/CLI/TutorialDecode.re.
 */

let default_prompt = "Work through the inline instructions in the editor below.";

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

/* Split input on `@prompt`/`@code`/`@test`/`@hint`/`@reference`/`@hints`/
   `@flags` marker lines. Default section is `code`. */
let parse_sections = (content: string): sections => {
  let lines = String.split_on_char('\n', content);
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

/* Filename -> module_name / title, matching the retired generator so the
   per-slide config tables (TutorialProbeStrip, TutorialSlideInit) keep
   their keys. */
let chop_lesson_ext = (rel: string): string =>
  Filename.check_suffix(rel, ".hzt")
    ? Filename.chop_suffix(rel, ".hzt") : Filename.chop_suffix(rel, ".hz");

let module_name_of = (rel: string): string => {
  let camel =
    String.split_on_char('/', chop_lesson_ext(rel))
    |> List.concat_map(String.split_on_char('-'))
    |> List.map(String.capitalize_ascii)
    |> String.concat("");
  "TuGen_" ++ camel;
};

let cap_words = (s: string): string =>
  String.split_on_char('-', s)
  |> List.filter(w => w != "")
  |> List.map(String.capitalize_ascii)
  |> String.concat(" ");

let starts_with_digit = (s: string): bool =>
  String.length(s) > 0
  && {
    let c = s.[0];
    c >= '0' && c <= '9';
  };

/* "01-holes" -> "01 - Holes"; a category token ("task"/"extra"/"bonus"),
   leading or right after a number, gets its own " - ": "task-grove-name" ->
   "Task - Grove Name", "26-task-grove-name" -> "26 - Task - Grove Name".
   Directory segments become " / "-joined prefixes. */
let is_category = (s: string): bool =>
  s == "task" || s == "extra" || s == "bonus";
let cap_join = (words: list(string)): string =>
  words
  |> List.filter(w => w != "")
  |> List.map(String.capitalize_ascii)
  |> String.concat(" ");
let title_of = (rel: string): string => {
  let segs = String.split_on_char('/', chop_lesson_ext(rel));
  switch (List.rev(segs)) {
  | [] => ""
  | [last, ...rev_dirs] =>
    let file_title =
      switch (String.split_on_char('-', last)) {
      | [num, cat, ...rest]
          when
            starts_with_digit(num)
            && is_category(cat)
            && List.exists(w => w != "", rest) =>
        num
        ++ " - "
        ++ String.capitalize_ascii(cat)
        ++ " - "
        ++ cap_join(rest)
      | [num, ...rest]
          when starts_with_digit(num) && List.exists(w => w != "", rest) =>
        num ++ " - " ++ cap_join(rest)
      | [cat, ...rest]
          when is_category(cat) && List.exists(w => w != "", rest) =>
        String.capitalize_ascii(cat) ++ " - " ++ cap_join(rest)
      | _ => cap_words(last)
      };
    let prefix =
      List.rev(rev_dirs) |> List.map(cap_words) |> String.concat(" / ");
    prefix == "" ? file_title : prefix ++ " / " ++ file_title;
  };
};

/* Deterministic per-index fallback id, for slides without an id= flag. */
let id_string = (i: int): string =>
  Printf.sprintf("%08x-7507-4000-8000-000000000000", 0x70000000 + i);

/* Fast-first text->zipper (FastParse, then the recovering parser); a
   total parse failure loads an empty buffer rather than failing boot. */
let zipper_of =
    (~label: string, ~rel: string, text: string): Haz3lcore.Zipper.t =>
  switch (
    Haz3lcore.PersistentZipper.parse_text(
      ~source=label ++ " of " ++ rel,
      ~root=Exp,
      text,
    )
  ) {
  | Some(z) => z
  | None =>
    print_endline("PARSE FAILED (" ++ label ++ " of " ++ rel ++ ")");
    Haz3lcore.Zipper.init();
  };

let spec_of = (i: int, (rel, raw): (string, string)): Tutorial.spec => {
  let s = parse_sections(raw);
  /* Slide sources carry editor indentation; strip it (Hazel re-indents). */
  let code = String.trim(Util.StringUtil.trim_leading(s.code));
  let test =
    s.test == "" ? "test true end" : Util.StringUtil.trim_leading(s.test);
  {
    id:
      Option.get(
        Haz3lcore.Id.of_string(Option.value(s.id, ~default=id_string(i))),
      ),
    title: title_of(rel),
    version: s.version,
    module_name: module_name_of(rel),
    prompt: s.prompt == "" ? default_prompt : s.prompt,
    display_hint: s.hint,
    task_reference: s.reference,
    your_impl:
      Haz3lcore.Zipper.caret_to_start(zipper_of(~label="@code", ~rel, code)),
    hidden_tests: {
      tests: zipper_of(~label="@test", ~rel, test),
      hints: s.hints,
    },
    wrapper: s.wrapper,
    show_report: s.show_report,
  };
};

let all: list(Tutorial.spec) = List.mapi(spec_of, Tutorialslides.Slides.all);
