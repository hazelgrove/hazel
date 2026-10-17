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

/* what of its item a cell holds */
[@deriving (show({with_path: false}), sexp, yojson)]
type kind =
  /* its pattern over its definition; [binds_module] for a `module` */
  | Def({binds_module: bool})
  /* a module's members (an opened or zoomed module row), MOD-rooted,
     under the module's name; the braces stay in the program */
  | Members
  /* a statement or the trailing expression: no header, its symbol
     shown instead */
  | Headless(string)
  /* one editor over a run of test statements: the item ids it covers,
     the first the cell's own */
  | Run(list(Haz3lcore.Id.t));

/* One open cell of a divided program: an item's header (pattern) and
   body (definition). The cell owns that text; the rest of the program
   stays in Divided's base. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  e_id: Haz3lcore.Id.t, /* the item tile's id in the program */
  e_kind: kind,
  /* header: pattern+signature, PAT- (or TPAT-)rooted */
  e_header: CellEditor.Model.t,
  /* body: the definition RHS, EXP- (or TYP-)rooted */
  e_body: CellEditor.Model.t,
  /* each one's text relative to its own left edge */
  e_header_indent: indent,
  e_body_indent: indent,
  e_ctx: Language.Ctx.t /* outer ctx at the definition */
};

/* which of a cell's two editors */
[@deriving (show({with_path: false}), sexp, yojson)]
type side =
  | Header
  | Body;

let pane = (side: side, e: t): CellEditor.Model.t =>
  switch (side) {
  | Header => e.e_header
  | Body => e.e_body
  };

let with_pane = (side: side, ed: CellEditor.Model.t, e: t): t =>
  switch (side) {
  | Header => {
      ...e,
      e_header: ed,
    }
  | Body => {
      ...e,
      e_body: ed,
    }
  };

/* what a cell without a header shows in its place */
let symbol = (e: t): option(string) =>
  switch (e.e_kind) {
  | Headless(sym) => Some(sym)
  | Run(_) => Some("tests")
  | Def(_)
  | Members => None
  };

let rec header_name = (e: t): option(string) =>
  switch (symbol(e)) {
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
  switch (e.e_kind) {
  | Run(ids) => ids
  | Def(_)
  | Members
  | Headless(_) => [e.e_id]
  };

let is_run = (e: t): bool =>
  switch (e.e_kind) {
  | Run(_) => true
  | Def(_)
  | Members
  | Headless(_) => false
  };
