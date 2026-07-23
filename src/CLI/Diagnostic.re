// Diagnostic.re: shared Rust-style diagnostic formatting for the CLI.
//
// Centralizes the source-location lookup, gutter/caret rendering, and
// severity-prefix machinery used by every command that surfaces statics
// output to the terminal. Callers supply the original program text, the
// path string used in headers, a Measured.t for id->position lookups, and
// either an `Info.t` (for the error/warning variants) or a raw message.

let lines_of_string = (s: string): array(string) => {
  /* Handle both \n and \r\n line endings */
  let s = Core.String.substr_replace_all(s, ~pattern="\r\n", ~with_="\n");
  Array.of_list(String.split_on_char('\n', s));
};

let make_caret_line = (col: int, len: int): string => {
  let spaces = String.make(col, ' ');
  let carets = String.make(max(1, len), '^');
  spaces ++ carets;
};

let warning_string = (item: Language.Warning.list_item): string =>
  switch (item) {
  | Pat(UnusedVar(name)) => "unused variable: " ++ name
  };

/* Format a single diagnostic (error or warning) in Rust-style with source
   context. `severity` is the header prefix ("error" / "warning"). `fallback`
   is appended after the header when no source position can be resolved
   (used by errors to print the offending term). */
let format_diagnostic_with_location =
    (
      ~severity: string,
      ~message: string,
      ~fallback: option(string)=?,
      ~source: string,
      ~path: string,
      measured: Haz3lcore.Measured.t,
      id: Util.Id.t,
    )
    : string => {
  let header = severity ++ ": " ++ message;
  switch (Haz3lcore.Measured.find_by_id(id, measured)) {
  | Some({origin, last}) =>
    let lines = lines_of_string(source);
    let row = origin.row;
    let col = origin.col;
    let len =
      if (origin.row == last.row) {
        last.col - origin.col;
      } else {
        1;
      };
    let line_num = row + 1;
    let line_num_str = string_of_int(line_num);
    let padding = String.make(String.length(line_num_str), ' ');
    let source_line =
      if (row >= 0 && row < Array.length(lines)) {
        lines[row];
      } else {
        "<source unavailable>";
      };
    let location =
      padding
      ++ " --> "
      ++ path
      ++ ":"
      ++ line_num_str
      ++ ":"
      ++ string_of_int(col + 1);
    let separator = padding ++ " |";
    let code_line = line_num_str ++ " | " ++ source_line;
    let caret_line = padding ++ " | " ++ make_caret_line(col, len);
    String.concat(
      "\n",
      [header, location, separator, code_line, caret_line],
    );
  | None =>
    switch (fallback) {
    | Some(extra) => header ++ "\n  " ++ extra
    | None => header
    }
  };
};

let format_error_with_location =
    (
      ~source: string,
      ~path: string,
      measured: Haz3lcore.Measured.t,
      info: Language.Info.t,
    )
    : option(string) =>
  Language.(
    switch (Info.marks_of(info)) {
    | [] => None
    | marks =>
      let message = Haz3lcore.ErrorPrint.string_of_marks(info, marks);
      let fallback = "in term: " ++ Haz3lcore.ErrorPrint.term_string_of(info);
      Some(
        format_diagnostic_with_location(
          ~severity="error",
          ~message,
          ~fallback,
          ~source,
          ~path,
          measured,
          Info.id_of(info),
        ),
      );
    }
  );

let format_warning_with_location =
    (
      ~source: string,
      ~path: string,
      measured: Haz3lcore.Measured.t,
      info: Language.Info.t,
    )
    : option(string) =>
  Language.(
    switch (Info.warnings_of(info)) {
    | [] => None
    | warnings =>
      let message =
        warnings |> List.map(warning_string) |> String.concat("; ");
      Some(
        format_diagnostic_with_location(
          ~severity="warning",
          ~message,
          ~source,
          ~path,
          measured,
          Info.id_of(info),
        ),
      );
    }
  );
