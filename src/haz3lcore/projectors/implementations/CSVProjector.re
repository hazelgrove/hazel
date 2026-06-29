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
  | Reset
  | Loaded(string) // a Pending(url) fetch returned this body
  | LoadFailed(string) // a Pending(url) fetch failed with this message
  | Reload; // re-fetch a url-loaded table (FileLoaded/Failed -> Pending)

/* Strip a leading UTF-8 byte-order mark (EF BB BF). Some tools prefix it to the
   first header; left in place it becomes part of that column's label, so a
   `data.`name`` projection silently fails to match. */
let strip_bom = (s: string): string =>
  String.length(s) >= 3
  && Char.code(s.[0]) == 0xEF
  && Char.code(s.[1]) == 0xBB
  && Char.code(s.[2]) == 0xBF
    ? String.sub(s, 3, String.length(s) - 3) : s;

/* Parsed CSV -> the table as a list-literal Exp: each row a labeled (or plain)
   tuple. Headers are sanitized into labels and the unnamed index column becomes
   `col<i>`; values are kept verbatim (string escaping happens at print time, not
   in the AST). This is the projector's canonical expansion — `initialize` hands
   it back as the resolved Exp, and `put` lifts it into editor syntax. */
let to_exp = (rows: CsvUtil.csv_data): Language.Exp.t => {
  module FE = Language.IdTagged.FreshGrammar.Exp;
  switch (rows) {
  | CsvUtil.WithHeaders(rows) =>
    FE.list_lit(
      List.map(
        (row: list((string, string))) =>
          FE.tuple(
            List.mapi(
              (i, (header: string, value: string)) => {
                let h =
                  StringUtil.sanitize_for_label(
                    String.trim(strip_bom(header)),
                  );
                let h = h == "" ? "col" ++ string_of_int(i) : h;
                FE.tup_label(FE.label(h), FE.string(value));
              },
              row,
            ),
          ),
        rows,
      ),
    )
  | CsvUtil.WithoutHeaders(rows) =>
    FE.list_lit(
      List.map(
        (row: list(string)) =>
          FE.tuple(List.map((value: string) => FE.string(value), row)),
        rows,
      ),
    )
  };
};

let put = (info, rows: CsvUtil.csv_data): Base.segment =>
  switch (
    info.utility.lift_syntax(
      ~inline=true,
      fun
      | Exp(any) =>
        Exp({
          ...any,
          term: Language.Exp.term_of(to_exp(rows)),
        })
      | _any => failwith("csv: put: not string literal"),
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("csv: put: lift failed")
  };

let reset_syntax = (info: info): Base.segment => {
  put(info, CsvUtil.WithoutHeaders([]));
};

/* Max columns of the filename shown in the loaded projector, a safety net for
   pathologically long filenames; normal filenames render in full. */
let display_max = 28;

/* The trailing filename of a file path or url: everything after the last '/',
   with any query string / fragment dropped. Local picker filenames (no '/')
   are returned as-is. */
let basename = (s: string): string => {
  let cut = (c, s) =>
    switch (String.index_opt(s, c)) {
    | Some(i) => String.sub(s, 0, i)
    | None => s
    };
  let s = s |> cut('?') |> cut('#');
  switch (String.rindex_opt(s, '/')) {
  | Some(i) when i + 1 < String.length(s) =>
    String.sub(s, i + 1, String.length(s) - i - 1)
  | _ => s
  };
};

/* Label shown in the loaded projector: just the filename, middle-truncated with
   an ellipsis only if it somehow exceeds display_max. */
let display_name = (s: string): string => {
  let s = basename(s);
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
   filename byte length (filenames are ascii) by display_max. */
let display_cols = (s: string): int =>
  min(String.length(basename(s)), display_max);

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
    // A pending fetch finished: load it, or record the failure. Results for a
    // model that's moved on (no longer Pending) are stale and ignored.
    | Loaded(content) =>
      switch (m) {
      | Pending(url) =>
        FileLoaded({
          filename: url,
          content,
          with_headers: true,
        })
      | _ => m
      }
    | LoadFailed(message) =>
      switch (m) {
      | Pending(url) =>
        Failed({
          url,
          message,
        })
      | _ => m
      }
    // Re-enter Pending so a frontend can re-invoke `resolve` to re-fetch
    // (manual reload). Resolution is one-shot, so this transition alone doesn't
    // re-fetch — the reload trigger must re-run resolution.
    | Reload =>
      switch (m) {
      | FileLoaded({filename, _}) => Pending(filename)
      | Failed({url, _}) => Pending(url)
      | _ => m
      }
    };
  };

  let error = (_, _): option(ProjectorBase.error) => None;

  /* What this model needs resolved: a Pending(url) fetches the url via the
   * injected UrlFetch hook (which handles consent / base-url resolution / local
   * files) and folds the outcome back as a Loaded / LoadFailed action. Settled
   * models need no resolution. */
  let resolve = (m: model): option(resolution(action)) =>
    switch (m) {
    | Pending(url) =>
      Some(
        k =>
          UrlFetch.get^(~url, ~on_done=res =>
            k(
              switch (res) {
              | Ok(content) => Loaded(content)
              | Error(message) => LoadFailed(message)
              },
            )
          ),
      )
    | NoFile
    | Failed(_)
    | FileLoaded(_) => None
    };

  /* This projector's contribution to the program: the loaded table built
   * directly as an Exp (never a segment). Unresolved / empty models contribute
   * nothing, so the driver leaves their underlying syntax in place. */
  let expand = (m: model, _info): option(Language.Exp.t) =>
    switch (m) {
    | FileLoaded({content, with_headers, _}) =>
      let rows =
        with_headers
          ? CsvUtil.WithHeaders(CsvUtil.parse_csv_with_headers(content))
          : CsvUtil.WithoutHeaders(
              CsvUtil.parse_csv_without_headers(content),
            );
      try(Some(to_exp(rows))) {
      | _ => None
      };
    | NoFile
    | Pending(_)
    | Failed(_) => None
    };
};
