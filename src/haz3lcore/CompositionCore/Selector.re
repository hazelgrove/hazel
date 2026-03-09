open Language;

/* Selector language for addressing Hazel syntax subtrees.
      See plans/Hazel-Agent-Path-Selector-Language.md for the full spec.

      Core operators:
      - `_`    : matches one syntactic slot
      - `_...` : matches zero or more slots along current spine
      - `⋱`/`\...` : descendant search (match P, then find Q inside)
      - `%`    : focus marker (selects the next syntactic unit)

      Binder-chain sugar:
      - `A/B/C` : navigate into binder A's def, then B's def, then resolve C
   */

/* === Surface AST === */

/* A token in the selector surface syntax */
type token =
  | Slot /* _ */
  | Ellipsis /* _... or … */
  | Focus /* % */
  | Descend /* ⋱ or \... */
  | KW_let
  | KW_fun
  | KW_if
  | KW_then
  | KW_else
  | KW_case
  | KW_end
  | KW_module
  | KW_type
  | KW_in
  | KW_test
  | Pipe /* | */
  | FatArrow /* => */
  | Equals /* = */
  | Colon /* : */
  | Arrow /* -> */
  | LBracket /* [ */
  | RBracket /* ] */
  | LParen /* ( */
  | RParen /* ) */
  | LBrace /* { */
  | RBrace /* } */
  | Semi /* ; */
  | Comma /* , */
  | Chain(list(string), bool) /* A/B/C - binder chain; bool = trailing slash */
  | Operator(string) /* binary operator: +, -, **, <, <=, >, >=, ==, !=, &&, ||, ++, etc. */
  | Literal(string) /* literal value: 42, 3.14, "hello", true, false */
  | Name(string) /* bare name */
  | NameIndex(string, int) /* x#0, x#1 - indexed name for shadowed bindings */
  | Index(int); /* #0, #1 - child index */

/* A parsed selector is a list of tokens */
type selector = list(token);

/* === Semantic (elaborated) selector === */

/* After elaboration, chains are expanded and the selector
   is structured for resolution against the term tree */
type sem_step =
  | MatchSlot /* consume one slot */
  | MatchEllipsis /* consume zero or more slots */
  | MatchFocus /* mark focus on next match */
  | MatchKeyword(string) /* match a keyword in the spine */
  | MatchDelimiter(string) /* match a delimiter like | or => */
  | MatchName(string) /* match a binder/identifier name */
  | MatchNameIndex(string, int) /* match nth binder with given name (0-based) */
  | MatchAtom(string) /* match a literal value by its string representation */
  | DescendInto /* descend into matched subtree */
  | EnterBinderDef(string) /* find binder by name, enter its def */
  | ChildIndex(int); /* descend into nth structural child */

type sem_selector = list(sem_step);

/* === Match result === */

type focus_target =
  | FocusExp(Exp.t)
  | FocusPat(Pat.t)
  | FocusTyp(Typ.t)
  | FocusMod(Mod.t);

type match_result = {
  focused: focus_target,
  focused_id: Id.t,
  /* breadcrumb for disambiguation */
  breadcrumb: string,
};

/* Helpers to construct match_result values concisely */
let mk_exp = (~bc="", e: Exp.t): match_result => {
  focused: FocusExp(e),
  focused_id: Exp.rep_id(e),
  breadcrumb: bc,
};

let mk_pat = (~bc="", p: Pat.t): match_result => {
  focused: FocusPat(p),
  focused_id: Pat.rep_id(p),
  breadcrumb: bc,
};

let mk_typ = (~bc="", t: Typ.t): match_result => {
  focused: FocusTyp(t),
  focused_id: Typ.rep_id(t),
  breadcrumb: bc,
};

let mk_mod = (~bc="", m: Mod.t): match_result => {
  focused: FocusMod(m),
  focused_id: Mod.rep_id(m),
  breadcrumb: bc,
};

/* === Tokenizer / Parser === */

/* Known binary operator symbols for selector matching.
   NOTE: / (division) is omitted due to conflict with chain (/) syntax.
   * (multiplication) can now be used since focus uses %. Will revisit later. */
let is_binop_token = (s: string): bool =>
  switch (s) {
  | "+"
  | "-"
  | "**"
  | "<"
  | "<="
  | ">"
  | ">="
  | "=="
  | "!="
  | "&&"
  | "||"
  | "++"
  | "$=="
  | "+."
  | "-."
  | "*."
  | "**."
  | "/."
  | "<."
  | "<=."
  | ">."
  | ">=."
  | "==."
  | "!=."
  | "::" => true
  | _ => false
  };

let tokenize = (input: string): list(token) => {
  /* Split on whitespace, then parse each token */
  let parts =
    input
    |> String.split_on_char(' ')
    |> List.filter(s => String.length(String.trim(s)) > 0);
  List.map(
    part =>
      switch (part) {
      | "_" => Slot
      | "_..."
      | "..."
      | "…" => Ellipsis /* … UTF-8 */
      | "%" => Focus
      | "\\..."
      | "⋱" => Descend /* ⋱ UTF-8 */
      | "let" => KW_let
      | "fun" => KW_fun
      | "if" => KW_if
      | "then" => KW_then
      | "else" => KW_else
      | "case" => KW_case
      | "end" => KW_end
      | "module" => KW_module
      | "type" => KW_type
      | "in" => KW_in
      | "test" => KW_test
      | "|" => Pipe
      | "=>" => FatArrow
      | "=" => Equals
      | ":" => Colon
      | "->" => Arrow
      | "[" => LBracket
      | "]" => RBracket
      | "(" => LParen
      | ")" => RParen
      | "{" => LBrace
      | "}" => RBrace
      | ";" => Semi
      | "," => Comma
      | s when is_binop_token(s) => Operator(s)
      | s when String.contains(s, '/') =>
        let len = String.length(s);
        let trailing_slash = len > 0 && s.[len - 1] == '/';
        let segments =
          s
          |> String.split_on_char('/')
          |> List.filter(seg => String.length(seg) > 0);
        switch (segments) {
        | [single] when !trailing_slash => Name(single)
        | segs => Chain(segs, trailing_slash)
        };
      | s when String.contains(s, '#') =>
        let parts = String.split_on_char('#', s);
        switch (parts) {
        /* #N syntax for child index (bare # prefix, no name) */
        | ["", idx_str] =>
          switch (int_of_string_opt(idx_str)) {
          | Some(idx) => Index(idx)
          | None => Name(s)
          }
        /* name#N syntax for indexed disambiguation */
        | [name, idx_str] when String.length(name) > 0 =>
          switch (int_of_string_opt(idx_str)) {
          | Some(idx) => NameIndex(name, idx)
          | None => Name(s)
          }
        | _ => Name(s)
        };
      /* Literal recognition: booleans, integers, floats, strings */
      | "true" => Literal("true")
      | "false" => Literal("false")
      | s when Token.is_string(s) => Literal(s)
      | s when Token.is_int(s) => Literal(s)
      | s when Token.is_float(s) => Literal(s)
      | s => Name(s)
      },
    parts,
  );
};

let parse = (input: string): selector => tokenize(input);

/* === Elaboration: surface -> semantic === */

let elaborate = (_sel: selector): sem_selector =>
  failwith("Selector.elaborate: not yet reimplemented");

/* === Resolution === */

/* Binder definitions can be expressions (let, module) or types (type alias).
   This sum type lets all binder-finding functions handle both uniformly. */
type binder_def =
  | ExpDef(Exp.t)
  | TypDef(Typ.t);

/* Resolve an elaborated semantic selector against an expression */
let resolve_sem = (_steps: sem_selector, _root: Exp.t): list(match_result) =>
  failwith("Selector.resolve_sem: not yet reimplemented");

/* Resolve a surface selector against an expression */
let resolve = (_sel: selector, _root: Exp.t): list(match_result) =>
  failwith("Selector.resolve: not yet reimplemented");

/* === Convenience: parse + resolve === */

let query = (_selector_str: string, _root: Exp.t): list(match_result) =>
  failwith("Selector.query: not yet reimplemented");

/* Format a semantic selector step as a readable string */
let step_to_string = (step: sem_step): string =>
  switch (step) {
  | MatchFocus => "%"
  | MatchSlot => "_"
  | MatchEllipsis => "_..."
  | MatchName(s) => s
  | MatchNameIndex(s, idx) => s ++ "#" ++ string_of_int(idx)
  | MatchAtom(s) => s
  | MatchKeyword(kw) => kw
  | MatchDelimiter(d) => d
  | EnterBinderDef(s) => s ++ "/"
  | DescendInto => "\\..."
  | ChildIndex(n) => "#" ++ string_of_int(n)
  };

/* Format a prefix of sem_selector steps as a readable string */
let steps_to_string = (steps: sem_selector): string =>
  steps |> List.map(step_to_string) |> String.concat(" ");

/* Diagnose why a selector produced no matches. */
let diagnose_no_match = (_selector_str: string, _root: Exp.t): string =>
  failwith("Selector.diagnose_no_match: not yet reimplemented");

/* For edit actions: require exactly one match */
let query_unique =
    (_selector_str: string, _root: Exp.t): result(match_result, string) =>
  failwith("Selector.query_unique: not yet reimplemented");

/* Print a match result's focused expression/pattern/type as text */
let print_match = (m: match_result): string => {
  let settings = ExpToSegment.Settings.of_core(~inline=true, CoreSettings.on);
  let segment =
    switch (m.focused) {
    | FocusExp(e) => ExpToSegment.exp_to_segment(~settings, e)
    | FocusPat(p) => ExpToSegment.pat_to_segment(~settings, p)
    | FocusTyp(t) => ExpToSegment.typ_to_segment(~settings, t)
    | FocusMod(item) => ExpToSegment.mod_to_segment(~settings, item)
    };
  Printer.of_segment(~holes="?", segment);
};

/* === Canonical path generation === */

let canonical_numeric = (_target: Id.t, _root: Exp.t): option(sem_selector) =>
  failwith("Selector.canonical_numeric: not yet reimplemented");

let canonical_named = (_target: Id.t, _root: Exp.t): option(sem_selector) =>
  failwith("Selector.canonical_named: not yet reimplemented");

/* Deparse: convert a sem_selector back to surface syntax string */
let deparse = (steps: sem_selector): string => {
  /* Collapse consecutive EnterBinderDef into chain syntax:
     EnterBinderDef(A), EnterBinderDef(B), MatchName(C) → "A/B/C"
     EnterBinderDef(A), EnterBinderDef(B), <other> → "A/B/" <other>
     EnterBinderDef(A), MatchName(B) → "A/B" */
  let rec go = (steps: sem_selector): list(string) =>
    switch (steps) {
    | [] => []
    | [EnterBinderDef(name), ...rest] =>
      let (chain_names, rest') = collect_chain([name], rest);
      switch (rest') {
      | [MatchName(last), ...rest''] =>
        let chain = String.concat("/", chain_names) ++ "/" ++ last;
        [chain, ...go(rest'')];
      | [MatchNameIndex(last, idx), ...rest''] =>
        let chain =
          String.concat("/", chain_names)
          ++ "/"
          ++ last
          ++ "#"
          ++ string_of_int(idx);
        [chain, ...go(rest'')];
      | _ =>
        let chain = String.concat("/", chain_names) ++ "/";
        [chain, ...go(rest')];
      };
    | [step, ...rest] =>
      let s =
        switch (step) {
        | MatchFocus => "%"
        | MatchSlot => "_"
        | MatchEllipsis => "_..."
        | MatchKeyword(kw) => kw
        | MatchDelimiter(d) => d
        | MatchName(n) => n
        | MatchNameIndex(n, i) => n ++ "#" ++ string_of_int(i)
        | MatchAtom(s) => s
        | ChildIndex(n) => "#" ++ string_of_int(n)
        | DescendInto => "\\..."
        | EnterBinderDef(_) => "" /* unreachable */
        };
      [s, ...go(rest)];
    }
  and collect_chain =
      (acc: list(string), steps: sem_selector)
      : (list(string), sem_selector) =>
    switch (steps) {
    | [EnterBinderDef(name), ...rest] => collect_chain(acc @ [name], rest)
    | _ => (acc, steps)
    };
  String.concat(" ", go(steps));
};
