/*
 * TutorialText: build Tutorial.spec records from the .hzt lesson sources
 * embedded by the tutorialslides library. This replaces the old
 * `./hazel gen-tutorial` codegen step: the .hzt files are compiled in as
 * raw text (ppx_blob) and parsed here at startup, so editing a slide is
 * just editing its .hzt and rebuilding.
 *
 * The .hzt marker format (see hazel-programs/tutorial/README.md):
 * @title/@prompt/@code/@test/@hint/@reference/@hints/@flags section lines;
 * the default (markerless) section is @code. The inverse direction
 * (spec -> text) lives in src/CLI/TutorialDecode.re.
 */

let default_prompt = "Work through the inline instructions in the editor below.";

type sections = {
  title: string,
  prompt: string,
  code: string,
  test: string,
  hint: string,
  reference: option(string),
  hints: list(string),
  wrapper: bool,
  show_report: bool,
  version: int,
  id: option(string),
};

let empty_sections = {
  title: "",
  prompt: "",
  code: "",
  test: "",
  hint: "",
  reference: None,
  hints: [],
  wrapper: false,
  show_report: false,
  version: 1,
  id: None,
};

let parse_flags = (s: sections, body: string): sections => {
  let toks =
    String.split(body, ~on='\n')
    |> List.concat_map(~f=String.split(~on=' '))
    |> List.map(~f=String.strip)
    |> List.filter(~f=t => !String.equal(t, ""));
  List.fold_left(
    ~f=
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
        | _
            when
              String.length(tok) > 8
              && String.equal(String.sub(tok, ~pos=0, ~len=8), "version=") => {
            ...acc,
            version:
              try(
                int_of_string(
                  String.sub(tok, ~pos=8, ~len=String.length(tok) - 8),
                )
              ) {
              | _ => acc.version
              },
          }
        | _
            when
              String.length(tok) > 3
              && String.equal(String.sub(tok, ~pos=0, ~len=3), "id=") => {
            ...acc,
            id: Some(String.sub(tok, ~pos=3, ~len=String.length(tok) - 3)),
          }
        | _ => acc
        },
    ~init=s,
    toks,
  );
};

/* The section a marker line opens. Text before any marker is `Code`.
   Adding a section here is a type error in `marker_of_line` until its marker
   text is given, and a section is read back out under the same constructor
   it was filed under, so there is no second spelling to keep in step. */
[@deriving eq]
type marker =
  | Title
  | Prompt
  | Code
  | Test
  | Hint
  | Reference
  | Hints
  | Flags;

let marker_of_line = (line: string): option(marker) =>
  switch (String.strip(line)) {
  | "@title" => Some(Title)
  | "@prompt" => Some(Prompt)
  | "@code" => Some(Code)
  | "@test" => Some(Test)
  | "@hint" => Some(Hint)
  | "@reference" => Some(Reference)
  | "@hints" => Some(Hints)
  | "@flags" => Some(Flags)
  | _ => None
  };

/* Every content line tagged with the section it fell in, in file order. */
let tag_lines = (content: string): list((marker, string)) =>
  String.split(content, ~on='\n')
  |> List.fold_left(
       ~f=
         ((tagged, cur), line) =>
           switch (marker_of_line(line)) {
           | Some(m) => (tagged, m)
           | None => ([(cur, line), ...tagged], cur)
           },
       ~init=([], Code),
     )
  |> fst
  |> List.rev;

/* One section's lines, newline-terminated, in file order; "" if it has none. */
let body = (tagged: list((marker, string)), m: marker): string =>
  tagged
  |> List.filter_map(~f=((m', line)) =>
       equal_marker(m, m') ? Some(line ++ "\n") : None
     )
  |> String.concat(~sep="");

let parse_sections = (content: string): sections => {
  let body = body(tag_lines(content));
  let trimmed_opt = (section: string): option(string) =>
    switch (String.strip(section)) {
    | "" => None
    | s => Some(s)
    };
  let s = {
    ...empty_sections,
    title: String.strip(body(Title)),
    prompt: String.strip(body(Prompt)),
    code: body(Code),
    test: String.strip(body(Test)),
    hint: String.strip(body(Hint)),
    reference: trimmed_opt(body(Reference)),
    hints:
      String.split(body(Hints), ~on='\n')
      |> List.map(~f=String.strip)
      |> List.filter(~f=h => !String.equal(h, "")),
  };
  parse_flags(s, body(Flags));
};

/* Filename -> module_name / title, matching the retired generator so the
   per-slide config tables (TutorialProbeStrip, TutorialSlideInit) keep
   their keys. */
let chop_lesson_ext = (rel: string): string =>
  Filename.check_suffix(rel, ".hzt")
    ? Filename.chop_suffix(rel, ".hzt") : Filename.chop_suffix(rel, ".hz");

let module_name_of = (rel: string): string => {
  let camel =
    String.split(chop_lesson_ext(rel), ~on='/')
    |> List.concat_map(~f=String.split(~on='-'))
    |> List.map(~f=String.capitalize)
    |> String.concat(~sep="");
  "TuGen_" ++ camel;
};

let cap_join = (words: list(string)): string =>
  words
  |> List.filter(~f=w => !String.equal(w, ""))
  |> List.map(~f=String.capitalize)
  |> String.concat(~sep=" ");

let cap_words = (s: string): string => cap_join(String.split(s, ~on='-'));

let starts_with_digit = (s: string): bool =>
  String.length(s) > 0
  && {
    let c = s.[0];
    Char.(c >= '0' && c <= '9');
  };

/* Fallback title when a slide has no `@title`: "01-holes" -> "01 - Holes";
   a category token ("task"/"extra") right after the number gets its own
   " - ": "26-task-grove-name" -> "26 - Task - Grove Name". Directory
   segments become the SlidePath folders the title sits in. */
let is_category = (s: string): bool =>
  String.equal(s, "task") || String.equal(s, "extra");
let title_of = (rel: string): string => {
  let segs = String.split(chop_lesson_ext(rel), ~on='/');
  switch (List.rev(segs)) {
  | [] => ""
  | [last, ...rev_dirs] =>
    let file_title =
      switch (String.split(last, ~on='-')) {
      | [num, cat, ...rest]
          when
            starts_with_digit(num)
            && is_category(cat)
            && List.exists(~f=w => !String.equal(w, ""), rest) =>
        num ++ " - " ++ String.capitalize(cat) ++ " - " ++ cap_join(rest)
      | [num, ...rest]
          when
            starts_with_digit(num)
            && List.exists(~f=w => !String.equal(w, ""), rest) =>
        num ++ " - " ++ cap_join(rest)
      | _ => cap_words(last)
      };
    let folders =
      List.rev(rev_dirs)
      |> List.map(~f=cap_words)
      |> List.filter(~f=f => !String.equal(f, ""));
    SlidePath.mk(~folders, file_title) |> SlidePath.to_string;
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
  /* Indentation is stored as whitespace in the editor, so preserve the
     leading spaces authored in the lesson source. */
  let code = String.strip(s.code);
  let test = String.equal(s.test, "") ? "test true end" : s.test;
  {
    id:
      Option.value_exn(
        Haz3lcore.Id.of_string(Option.value(s.id, ~default=id_string(i))),
      ),
    title: String.equal(s.title, "") ? title_of(rel) : s.title,
    version: s.version,
    module_name: module_name_of(rel),
    prompt: String.equal(s.prompt, "") ? default_prompt : s.prompt,
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

let all: list(Tutorial.spec) =
  List.mapi(~f=spec_of, Tutorialslides.Slides.all);
