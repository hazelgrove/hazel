open Util;

/* how a cell's text sat in the program: [base] columns of indentation
   were cut from every line after the first, and [cut]/[orig] are the
   text as cut and as it was, so a splice hands back the program's own
   pieces wherever the cell left them alone */
[@deriving (show({with_path: false}), sexp, yojson)]
type indent = {
  base: int,
  cut: Haz3lcore.Segment.t,
  orig: Haz3lcore.Segment.t,
};

let no_indent: indent = {
  base: 0,
  cut: [],
  orig: [],
};

/* One open cell of a divided program: an item's header (pattern) and
   body (definition). The cell owns that text; the rest of the program
   stays in Divided's base. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  e_id: Haz3lcore.Id.t, /* the item tile's id in the program */
  /* header: pattern+signature, PAT- (or TPAT-)rooted */
  e_header: CellEditor.Model.t,
  /* module items: private pat statics would misread the MPat binder as
     a constructor, so the header only takes its item's projected ones */
  e_mod: bool,
  /* headerless items (statements, the trailing expression): the
     static symbol shown instead of a header cell */
  e_sym: option(string),
  /* a run cell: one editor spanning a contiguous run of test
     statements, anchored at the first test's item id */
  e_run: bool,
  /* run cells: the item ids the run covers (first = e_id) */
  e_members: list(Haz3lcore.Id.t),
  /* a module's members (an opened or zoomed module row): the body is
     its members, MOD-rooted, and the braces stay in the program */
  e_inner: bool,
  /* body: the definition RHS, EXP- (or TYP-)rooted */
  e_body: CellEditor.Model.t,
  /* each one's text relative to its own left edge */
  e_header_indent: indent,
  e_body_indent: indent,
  e_ctx: Language.Ctx.t /* outer ctx at the definition */
};

let rec header_name = (e: t): option(string) =>
  switch (e.e_sym) {
  | Some(sym) => Some(sym)
  | None => header_name_of_cell(e)
  }
and header_name_of_cell = (e: t): option(string) => {
  let txt =
    Haz3lcore.MarkerParse.to_text(e.e_header.editor.editor.state.zipper);
  let txt = String.trim(txt);
  /* any byte of a non-ASCII character counts: `café` stays whole */
  let ident = c =>
    c >= 'a'
    && c <= 'z'
    || c >= 'A'
    && c <= 'Z'
    || c >= '0'
    && c <= '9'
    || c == '_'
    || c == '\''
    || Char.code(c) >= 0x80;
  let rec run = i =>
    i < String.length(txt) && ident(txt.[i]) ? run(i + 1) : i;
  /* `add(x: Int): Int` is add; a pattern header like `(p, q)` keeps its
     text up to an annotation */
  let name =
    switch (run(0)) {
    | 0 =>
      switch (String.index_opt(txt, ':')) {
      | Some(i) => String.trim(String.sub(txt, 0, i))
      | None => txt
      }
    | n => String.sub(txt, 0, n)
    };
  name == "" ? None : Some(name);
};

let covers = (e: t): list(Haz3lcore.Id.t) =>
  e.e_run ? e.e_members : [e.e_id];
