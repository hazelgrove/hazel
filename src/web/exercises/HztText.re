/*
 * HztText: the section syntax shared by Hazel's .hzt sources -- tutorial
 * lessons (TutorialText, hazel-programs/tutorial) and exercises
 * (ExerciseText, hazel-programs/exercises).
 *
 * A file is a sequence of sections. A marker line opens one: a line whose
 * trimmed text is `@name`, or `@name <argument>` for a marker that takes an
 * argument (an exercise's `@bug <hint>`). Every line after it, up to the
 * next marker line, is the section's body. Lines before the first marker
 * belong to the reader's default section.
 *
 * Only names the reader knows are markers, so a program line that happens
 * to begin with `@` (Hazel's list append) is body text.
 */

type section('m) = {
  marker: 'm,
  /* the trimmed text after `@name ` on the marker line; "" if none */
  arg: string,
  /* the body, one entry per line, in file order */
  lines: list(string),
};

/* `@name` / `@name arg` split into (name, arg), or None if the line is not
   shaped like a marker line. */
let split_marker_line = (line: string): option((string, string)) => {
  let t = String.trim(line);
  let n = String.length(t);
  if (n < 2 || t.[0] != '@') {
    None;
  } else {
    switch (String.index_opt(t, ' ')) {
    | None => Some((String.sub(t, 1, n - 1), ""))
    | Some(i) =>
      Some((String.sub(t, 1, i - 1), String.trim(String.sub(t, i, n - i))))
    };
  };
};

/* The file's sections in order. `marker_of(name, arg)` says whether
   `@name` (with the rest of its line, `arg`) opens a section, and which.
   A file with text before its first marker starts with a `default`
   section; one without starts with its first marker. */
let sections =
    (
      ~marker_of: (string, string) => option('m),
      ~default: 'm,
      content: string,
    )
    : list(section('m)) => {
  let close = (cur, acc) => [
    {
      ...cur,
      lines: List.rev(cur.lines),
    },
    ...acc,
  ];
  let (acc, cur) =
    content
    |> Util.StringUtil.replace(Util.StringUtil.regexp("\r\n?"), _, "\n")
    |> String.split_on_char('\n')
    |> List.fold_left(
         ((acc, cur), line) =>
           switch (
             Option.bind(split_marker_line(line), ((name, arg)) =>
               marker_of(name, arg) |> Option.map(m => (m, arg))
             )
           ) {
           | Some((marker, arg)) => (
               close(cur, acc),
               {
                 marker,
                 arg,
                 lines: [],
               },
             )
           | None => (
               acc,
               {
                 ...cur,
                 lines: [line, ...cur.lines],
               },
             )
           },
         (
           [],
           {
             marker: default,
             arg: "",
             lines: [],
           },
         ),
       );
  switch (List.rev(close(cur, acc))) {
  /* an empty preamble is not a section */
  | [{lines, _}, ...rest]
      when List.for_all(l => String.trim(l) == "", lines) => rest
  | all => all
  };
};

let is_blank = (line: string): bool => String.trim(line) == "";

/* Prose (a title, a markdown prompt): the lines, trimmed as a whole. */
let prose = (lines: list(string)): string =>
  String.trim(String.concat("\n", lines));

/* Program text: the lines without the blank lines that separate sections
   (leading and trailing ones), and otherwise as written -- indentation is
   the text's own spaces. */
let code = (lines: list(string)): string => {
  let rec drop_blank =
    fun
    | [l, ...ls] when is_blank(l) => drop_blank(ls)
    | ls => ls;
  lines
  |> drop_blank
  |> List.rev
  |> drop_blank
  |> List.rev
  |> String.concat("\n");
};

/* Space/newline-separated tokens (a `@flags` or `@points` body). */
let tokens = (lines: list(string)): list(string) =>
  lines
  |> List.concat_map(String.split_on_char(' '))
  |> List.map(String.trim)
  |> List.filter(t => t != "");

/* `key=value` -> Some((key, value)); a token without `=` -> None. */
let key_value = (token: string): option((string, string)) =>
  switch (String.index_opt(token, '=')) {
  | None => None
  | Some(i) =>
    Some((
      String.sub(token, 0, i),
      String.sub(token, i + 1, String.length(token) - i - 1),
    ))
  };

/* Fast-first text->zipper (FastParse, then the recovering parser); a
   total parse failure loads an empty buffer rather than failing boot. */
let zipper_of = (~source: string, text: string): Haz3lcore.Zipper.t =>
  switch (Haz3lcore.PersistentZipper.parse_text(~source, ~root=Exp, text)) {
  | Some(z) => z
  | None =>
    print_endline("PARSE FAILED (" ++ source ++ ")");
    Haz3lcore.Zipper.init();
  };
