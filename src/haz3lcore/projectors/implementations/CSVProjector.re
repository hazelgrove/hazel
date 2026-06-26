open Util;
open ProjectorBase;

/* CSV projector logic: loads a CSV file into a list of (optionally
   labeled) tuples. The web view (file-select button, header toggle)
   lives in src/web/projectors/CSVProjectorView.re, reusing the
   helpers below. */

[@deriving (show({with_path: false}), sexp, yojson)]
type model_t =
  | NoFile // No file selected (file-picker entry point)
  | Pending(string) // url: data being fetched at init time, not yet resolved
  | Failed({
      url: string,
      message: string,
    }) // url: fetch or parse failed
  | FileLoaded({
      filename: string,
      content: string,
      with_headers: bool,
    }); // data loaded (from a picked file or a fetched url) with header toggle

[@deriving (show({with_path: false}), sexp, yojson)]
type action_t =
  | SetFile({
      filename: string,
      content: string,
    }) // web file-picker selected a local file
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

/* Max columns of the file/url name shown in the loaded projector. Keeps both
   the inline badge and the editor space reserved for it compact even for long
   urls. */
let display_max = 28;

let strip_scheme = (s: string): string =>
  if (String.starts_with(~prefix="https://", s)) {
    String.sub(s, 8, String.length(s) - 8);
  } else if (String.starts_with(~prefix="http://", s)) {
    String.sub(s, 7, String.length(s) - 7);
  } else {
    s;
  };

/* Human-readable label for a loaded file/url: drop the scheme and, if still too
   long, middle-truncate with an ellipsis so both the host and the trailing
   filename stay visible. Used by the web view for the badge text. */
let display_name = (s: string): string => {
  let s = strip_scheme(s);
  if (String.length(s) <= display_max) {
    s;
  } else {
    let keep = display_max - 1; /* room for the ellipsis */
    let head = keep / 2;
    let tail = keep - head;
    String.sub(s, 0, head)
    ++ "…"
    ++ String.sub(s, String.length(s) - tail, tail);
  };
};

/* Column width display_name occupies. The ellipsis is one column but three
   bytes, so we can't String.length the truncated result; instead bound the
   scheme-stripped byte length (urls are ascii) by display_max. */
let display_cols = (s: string): int =>
  min(String.length(strip_scheme(s)), display_max);

module M: Projector with type model = model_t and type action = action_t = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = model_t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = action_t;

  let init = (a: Language.Any.t): option(model) => {
    switch (a) {
    // ^^csv("https://...") — a string literal is a url to fetch at init time
    | Exp({term: Atom(String(url)), _}) => Some(Pending(url))
    | Exp({term: ListLit([]), _}) => Some(NoFile) // No file selected
    | _ => None
    };
  };

  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (m, _) =>
    switch (m) {
    | FileLoaded({filename, _}) =>
      ProjectorCore.Shape.inline(display_cols(filename) + 6) // Account for reset button and toggle
    | Pending(_) => ProjectorCore.Shape.inline(11) // "Loading … "
    | Failed(_) => ProjectorCore.Shape.inline(13)
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

  /* Initialization phase: when the underlying syntax was a url string,
   * `init` left us in `Pending(url)`. Fetch the url via the injected
   * UrlFetch hook and, on success, splice the parsed CSV in as the
   * projector's syntax (reusing `put`, exactly as the web file-picker does).
   * Already-resolved models (NoFile / FileLoaded / Failed) need no work. */
  let initialize =
    Some(
      (
        model: model,
        info,
        ~k: (option(model), option(Base.segment)) => unit,
      ) =>
        switch (model) {
        | Pending(url) =>
          UrlFetch.get^(~url, ~on_done=res =>
            switch (res) {
            | Ok(content) =>
              let seg =
                try(
                  Some(
                    put(
                      info,
                      CsvUtil.WithHeaders(
                        CsvUtil.parse_csv_with_headers(content),
                      ),
                    ),
                  )
                ) {
                | _ => None
                };
              switch (seg) {
              | Some(seg) =>
                k(
                  Some(
                    FileLoaded({
                      filename: url,
                      content,
                      with_headers: true,
                    }),
                  ),
                  Some(seg),
                )
              | None =>
                k(
                  Some(
                    Failed({
                      url,
                      message: "could not parse CSV from " ++ url,
                    }),
                  ),
                  None,
                )
              };
            | Error(message) =>
              k(
                Some(
                  Failed({
                    url,
                    message,
                  }),
                ),
                None,
              )
            }
          );
          true;
        | NoFile
        | Failed(_)
        | FileLoaded(_) => false
        },
    );
};
