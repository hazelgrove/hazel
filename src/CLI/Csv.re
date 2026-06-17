open Util;

/* Edit-time CSV ingestion for the Hazel CLI.

   A `.hz` source may reference a CSV file with the form

       ^^csv("relative/or/absolute/path.csv")

   `expand` rewrites each such reference into an inline `^^table([...])`
   literal whose cells are all String values (matching the convention used by
   grade.hz and the in-editor CSV projector). The rewrite happens *before* the
   program is parsed, so the language itself performs no I/O. Reading a file is
   a tooling step gated by the `authorize` callback supplied by the caller (the
   CLI prompts the user by default).

   Parsing reuses Util.CsvUtil (the `csv` opam library, RFC-4180 with quoted
   fields / embedded commas) and value/label sanitizing reuses Util.StringUtil
   — the same helpers used by the in-editor CSV projector, see
   src/haz3lcore/projectors/implementations/CSVProjector.re. */

/* What an authorizer decides for one `^^csv(...)` reference. */
type decision =
  | Allow(string) /* read this (possibly user-substituted) path */
  | Deny;

/* Resolve a referenced path: absolute paths are used as-is, relative paths
   are taken relative to `base_dir` (the .hz file's directory, or --data-dir). */
let resolve = (~base_dir: string, path: string): string =>
  Filename.is_relative(path) ? Filename.concat(base_dir, path) : path;

let read_file = (path: string): string => {
  let ic = open_in_bin(path);
  let n = in_channel_length(ic);
  let s = really_input_string(ic, n);
  close_in(ic);
  s;
};

/* ---- Emitting a `^^table([...])` literal from parsed CSV rows ---- */

/* A Hazel label, backtick-quoted so capitalized / spaced / punctuated CSV
   headers (e.g. `Fare`, `No. of cases`) are all legal. The unnamed index
   column pandas writes (empty header) becomes `col<i>`. */
let label_literal = (~index: int, header: string): string => {
  let h = StringUtil.sanitize_for_label(header);
  let h = h == "" ? "col" ++ string_of_int(index) : h;
  "`" ++ h ++ "`";
};

/* A Hazel String literal for a cell value. */
let string_literal = (value: string): string =>
  "\"" ++ StringUtil.sanitize_for_string_expression(value) ++ "\"";

/* One CSV row -> a labeled-tuple literal: (`H1`="v1", `H2`="v2", ...). */
let row_literal = (row: list((string, string))): string => {
  let fields =
    List.mapi(
      (i, (header, value)) =>
        label_literal(~index=i, header) ++ "=" ++ string_literal(value),
      row,
    );
  "(" ++ String.concat(", ", fields) ++ ")";
};

/* CSV text -> a `^^table([...])` literal with all-String cells. */
let to_table_literal = (csv: string): string => {
  let rows = CsvUtil.parse_csv_with_headers(csv);
  let row_lits = List.map(row_literal, rows);
  "^^table([\n  " ++ String.concat(",\n  ", row_lits) ++ "\n])";
};

/* ---- Scanning a source for `^^csv("path")` references ---- */

let is_ident_char = (c: char): bool =>
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c == '_';

let is_ws = (c: char): bool =>
  c == ' ' || c == '\t' || c == '\n' || c == '\r';

let skip_ws = (s: string, i: int): int => {
  let n = String.length(s);
  let j = ref(i);
  while (j^ < n && is_ws(s.[j^])) {
    j := j^ + 1;
  };
  j^;
};

/* Parse a double-quoted string literal starting at index i (the char there is
   a double quote), honoring backslash-escaped quotes and backslashes. Returns
   the decoded string and the index just past the closing quote. */
let parse_string_lit = (s: string, i: int): option((string, int)) => {
  let n = String.length(s);
  let buf = Buffer.create(32);
  let j = ref(i + 1);
  let result = ref(None);
  let stop = ref(false);
  while (! stop^ && j^ < n) {
    let c = s.[j^];
    if (c == '\\' && j^ + 1 < n) {
      Buffer.add_char(buf, s.[j^ + 1]);
      j := j^ + 2;
    } else if (c == '"') {
      result := Some((Buffer.contents(buf), j^ + 1));
      stop := true;
    } else {
      Buffer.add_char(buf, c);
      j := j^ + 1;
    };
  };
  result^;
};

let marker = "^^csv";

/* If `^^csv ( "path" )` (whitespace-tolerant) starts at index `i`, return the
   referenced path and the index just past the closing `)`. The char after the
   marker must not continue an identifier, so `^^csvother(...)` is left alone. */
let match_csv_call = (s: string, i: int): option((string, int)) => {
  let n = String.length(s);
  let mlen = String.length(marker);
  if (i + mlen <= n
      && String.sub(s, i, mlen) == marker
      && (i + mlen >= n || !is_ident_char(s.[i + mlen]))) {
    let p1 = skip_ws(s, i + mlen);
    if (p1 < n && s.[p1] == '(') {
      let p2 = skip_ws(s, p1 + 1);
      if (p2 < n && s.[p2] == '"') {
        switch (parse_string_lit(s, p2)) {
        | Some((path, p3)) =>
          let p4 = skip_ws(s, p3);
          p4 < n && s.[p4] == ')' ? Some((path, p4 + 1)) : None;
        | None => None
        };
      } else {
        None;
      };
    } else {
      None;
    };
  } else {
    None;
  };
};

/* Rewrite every `^^csv("path")` reference in `src` into an inline table
   literal. `authorize` is called with the declared path for each reference and
   returns the path to actually read, or Deny. Denial raises Failure.

   This produces TEXT (used by `hazel expand` to materialize a self-contained
   .hz). The materialized table is large, so re-parsing it is slow; for direct
   evaluation prefer `splice_tables` below, which never re-parses the table. */
let expand =
    (~base_dir: string, ~authorize: string => decision, src: string): string => {
  let n = String.length(src);
  let buf = Buffer.create(n);
  let i = ref(0);
  while (i^ < n) {
    switch (src.[i^] == '^' ? match_csv_call(src, i^) : None) {
    | Some((path, next)) =>
      switch (authorize(path)) {
      | Allow(actual) =>
        let full = resolve(~base_dir, actual);
        Buffer.add_string(buf, to_table_literal(read_file(full)));
      | Deny => failwith("Reading CSV \"" ++ path ++ "\" was not authorized.")
      };
      i := next;
    | None =>
      Buffer.add_char(buf, src.[i^]);
      i := i^ + 1;
    };
  };
  Buffer.contents(buf);
};

/* ---- Fast path: splice tables as AST, never re-parsing the table ----

   Parsing a large inlined `^^table([...])` literal through the editor parser
   is expensive. Instead we replace each `^^csv("path")` with a short fresh
   variable, parse only the small skeleton, build each table directly as AST
   (the same shape the in-editor CSV projector produces), and bind it with a
   wrapping `let`. The big table never goes through the parser. */

module FE = Language.IdTagged.FreshGrammar.Exp;
module FP = Language.IdTagged.FreshGrammar.Pat;

/* Replace each `^^csv("path")` with a fresh `__hz_csv_<k>` variable. Returns
   the rewritten (small) source and the (var, path) references in source order. */
let extract_refs = (src: string): (string, list((string, string))) => {
  let n = String.length(src);
  let buf = Buffer.create(n);
  let refs = ref([]);
  let k = ref(0);
  let i = ref(0);
  while (i^ < n) {
    switch (src.[i^] == '^' ? match_csv_call(src, i^) : None) {
    | Some((path, next)) =>
      let var = "__hz_csv_" ++ string_of_int(k^);
      k := k^ + 1;
      refs := [(var, path), ...refs^];
      Buffer.add_string(buf, var);
      i := next;
    | None =>
      Buffer.add_char(buf, src.[i^]);
      i := i^ + 1;
    };
  };
  (Buffer.contents(buf), List.rev(refs^));
};

/* One CSV row -> a labeled-tuple AST node. Labels are sanitized (and the
   unnamed index column becomes `col<i>`); values are kept verbatim. */
let row_ast = (row: list((string, string))): Language.Exp.t =>
  FE.tuple(
    List.mapi(
      (i, (header, value)) => {
        let h = StringUtil.sanitize_for_label(header);
        let h = h == "" ? "col" ++ string_of_int(i) : h;
        FE.tup_label(FE.label(h), FE.string(value));
      },
      row,
    ),
  );

/* CSV text -> a `[ (..), (..), .. ]` list-literal AST. */
let table_ast = (csv: string): Language.Exp.t =>
  FE.list_lit(List.map(row_ast, CsvUtil.parse_csv_with_headers(csv)));

let load_table_ast =
    (~base_dir: string, ~authorize: string => decision, path: string)
    : Language.Exp.t =>
  switch (authorize(path)) {
  | Allow(actual) => table_ast(read_file(resolve(~base_dir, actual)))
  | Deny => failwith("Reading CSV \"" ++ path ++ "\" was not authorized.")
  };

/* Bind every CSV reference with a wrapping `let __hz_csv_k = <table> in ...`
   around an already-parsed body term. Prompts (via authorize) fire in source
   order. */
let wrap_lets =
    (
      ~base_dir: string,
      ~authorize: string => decision,
      refs: list((string, string)),
      body: Language.Exp.t,
    )
    : Language.Exp.t => {
  let loaded =
    List.map(
      ((var, path)) => (var, load_table_ast(~base_dir, ~authorize, path)),
      refs,
    );
  List.fold_right(
    ((var, ast), acc) => FE.let_(FP.var(var), ast, acc),
    loaded,
    body,
  );
};

/* Expand `^^csv("...")` references by parsing only the skeleton and splicing
   tables as AST. `parse` is the caller's text -> term function. */
let splice_tables =
    (
      ~base_dir: string,
      ~authorize: string => decision,
      ~parse: string => Language.Exp.t,
      src: string,
    )
    : Language.Exp.t => {
  let (skeleton, refs) = extract_refs(src);
  wrap_lets(~base_dir, ~authorize, refs, parse(skeleton));
};
