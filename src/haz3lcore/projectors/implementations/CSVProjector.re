open Util;
open ProjectorBase;

/* CSV projector logic: loads a CSV file into a list of (optionally
   labeled) tuples. The web view (file-select button, header toggle)
   lives in src/web/projectors/CSVProjectorView.re, reusing the
   helpers below. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model_t =
  | NoFile // No file selected
  | FileLoaded({
      filename: string,
      content: string,
      with_headers: bool,
    }); // File loaded with header toggle

[@deriving (show({with_path: false}), sexp, yojson)]
type action_t =
  | SetFile({
      filename: string,
      content: string,
    })
  | ToggleHeaders
  | Reset;

let put = (info, rows: CsvUtil.csv_data): Base.segment => {
  let exp: Language.Exp.term =
    switch (rows) {
    | CsvUtil.WithHeaders(rows) =>
      ListLit(
        List.map(
          (row: list((string, string))) =>
            Language.IdTagged.FreshGrammar.Exp.(
              tuple(
                List.map(
                  ((header: string, value: string)) =>
                    tup_label(
                      label(StringUtil.sanitize_for_label(header)),
                      string(
                        StringUtil.sanitize_for_string_expression(value),
                      ),
                    ),
                  row,
                ),
              )
            ),
          rows,
        ),
      )
    | CsvUtil.WithoutHeaders(rows) =>
      ListLit(
        List.map(
          (row: list(string)) =>
            Language.IdTagged.FreshGrammar.Exp.(
              tuple(
                List.map(
                  (value: string) =>
                    string(StringUtil.sanitize_for_string_expression(value)),
                  row,
                ),
              )
            ),
          rows,
        ),
      )
    };

  switch (
    info.utility.lift_syntax(
      ~inline=true,
      fun
      | Exp(any) =>
        Exp({
          ...any,
          term: exp,
        })
      | _any => failwith("csv: put: not string literal"),
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("csv: put: lift failed")
  };
};

let reset_syntax = (info: info): Base.segment => {
  put(info, CsvUtil.WithoutHeaders([]));
};

module M: Projector with type model = model_t and type action = action_t = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = model_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = action_t;

  let init = (a: Language.Any.t): option(model) => {
    switch (a) {
    | Exp({term: ListLit([]), _}) => Some(NoFile) // No file selected
    | _ => None
    };
  };

  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (m, _) =>
    switch (m) {
    | FileLoaded({filename, _}) =>
      ProjectorCore.Shape.inline(String.length(filename) + 6) // Account for reset button and toggle
    | NoFile => ProjectorCore.Shape.inline(13)
    };
  let update = (m: model, _, action: action) => {
    switch (action) {
    | SetFile({filename, content}) =>
      FileLoaded({
        filename,
        content,
        with_headers: true,
      })
    | ToggleHeaders =>
      switch (m) {
      | FileLoaded({filename, content, with_headers}) =>
        FileLoaded({
          filename,
          content,
          with_headers: !with_headers,
        })
      | _ => m
      }
    | Reset => NoFile
    };
  };

  let error = (_, _): option(ProjectorBase.error) => None;
};
