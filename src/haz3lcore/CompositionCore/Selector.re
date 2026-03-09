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

let elaborate = (sel: selector): sem_selector => {
  let rec go = (tokens: selector): sem_selector =>
    switch (tokens) {
    | [] => []
    | [Slot, ...rest] => [MatchSlot, ...go(rest)]
    | [Ellipsis, ...rest] => [MatchEllipsis, ...go(rest)]
    | [Focus, ...rest] => [MatchFocus, ...go(rest)]
    | [Descend, Descend, ...rest] => go([Descend, ...rest]) /* idempotent */
    | [Descend, ...rest] => [DescendInto, ...go(rest)]
    | [KW_let, ...rest] => [MatchKeyword("let"), ...go(rest)]
    | [KW_fun, ...rest] => [MatchKeyword("fun"), ...go(rest)]
    | [KW_if, ...rest] => [MatchKeyword("if"), ...go(rest)]
    | [KW_then, ...rest] => [MatchKeyword("then"), ...go(rest)]
    | [KW_else, ...rest] => [MatchKeyword("else"), ...go(rest)]
    | [KW_case, ...rest] => [MatchKeyword("case"), ...go(rest)]
    | [KW_end, ...rest] => [MatchKeyword("end"), ...go(rest)]
    | [KW_module, ...rest] => [MatchKeyword("module"), ...go(rest)]
    | [KW_type, ...rest] => [MatchKeyword("type"), ...go(rest)]
    | [KW_in, ...rest] => [MatchKeyword("in"), ...go(rest)]
    | [KW_test, ...rest] => [MatchKeyword("test"), ...go(rest)]
    | [Pipe, ...rest] => [MatchDelimiter("|"), ...go(rest)]
    | [FatArrow, ...rest] => [MatchDelimiter("=>"), ...go(rest)]
    | [Equals, ...rest] => [MatchDelimiter("="), ...go(rest)]
    | [Colon, ...rest] => [MatchDelimiter(":"), ...go(rest)]
    | [Arrow, ...rest] => [MatchDelimiter("->"), ...go(rest)]
    | [LBracket, ...rest] => [MatchDelimiter("["), ...go(rest)]
    | [RBracket, ...rest] => [MatchDelimiter("]"), ...go(rest)]
    | [LParen, ...rest] => [MatchDelimiter("("), ...go(rest)]
    | [RParen, ...rest] => [MatchDelimiter(")"), ...go(rest)]
    | [LBrace, ...rest] => [MatchDelimiter("{"), ...go(rest)]
    | [RBrace, ...rest] => [MatchDelimiter("}"), ...go(rest)]
    | [Semi, ...rest] => [MatchDelimiter(";"), ...go(rest)]
    | [Chain(segments, trailing_slash), ...rest] =>
      /* A/B/C  (no trailing slash): EnterBinderDef(A), EnterBinderDef(B), MatchName(C)
         A/B/C/ (trailing slash):    EnterBinderDef(A), EnterBinderDef(B), EnterBinderDef(C) */
      let chain_steps =
        if (trailing_slash) {
          List.map(s => EnterBinderDef(s), segments);
        } else {
          switch (List.rev(segments)) {
          | [] => []
          | [last, ...rev_init] =>
            let init = List.rev(rev_init);
            List.map(s => EnterBinderDef(s), init) @ [MatchName(last)];
          };
        };
      chain_steps @ go(rest);
    | [Operator(op), ...rest] => [MatchDelimiter(op), ...go(rest)]
    | [NameIndex(name, idx), ...rest] => [
        MatchNameIndex(name, idx),
        ...go(rest),
      ]
    | [Literal(s), ...rest] => [MatchAtom(s), ...go(rest)]
    | [Index(n), ...rest] => [ChildIndex(n), ...go(rest)]
    | [Name(s), ...rest] => [MatchName(s), ...go(rest)]
    };
  let steps = go(sel);
  /* Implicit focus: if no MatchFocus in the selector, append one.
     This means selectors like `A/B/C/` or `let x` produce a result
     without requiring an explicit `%`. */
  if (List.exists(s => s == MatchFocus, steps)) {
    steps;
  } else {
    /* If the last step is a name, insert MatchFocus before it
       (focus on the last-mentioned term). Otherwise append. */
    switch (List.rev(steps)) {
    | [MatchName(_) as last, ...rev_rest]
    | [MatchNameIndex(_, _) as last, ...rev_rest]
    | [MatchAtom(_) as last, ...rev_rest] =>
      List.rev(rev_rest) @ [MatchFocus, last]
    | _ => steps @ [MatchFocus]
    };
  };
};

/* === Resolution === */

/* Helper: get the pattern name from a Pat.t */
let pat_name = (p: Pat.t): option(string) =>
  /* Use Pat.is_var which handles Asc/Parens/Projector/TupLabel wrappers */
  Pat.is_var(p);

/* Helper: get the tpat name from a TPat.t */
let tpat_name = (tp: TPat.t): option(string) =>
  switch (tp.term) {
  | Var(name) => Some(name)
  | _ => None
  };

/* Helper: get the module pattern name from an mpat_t */
let rec mpat_name = (mp: TermBase.mpat_t): option(string) =>
  switch (mp.term) {
  | Var(name) => Some(name)
  | Asc(mp, _) => mpat_name(mp)
  | _ => None
  };

/* Helper: convert a TPat.t to a Pat.t for mk_pat results.
   Preserves the ID from the TPat. */
let tpat_to_pat = (tp: TPat.t): Pat.t =>
  switch (tp.term) {
  | Var(name) => IdTagged.fast_copy(TPat.rep_id(tp), Pat.fresh(Var(name)))
  | _ => IdTagged.fast_copy(TPat.rep_id(tp), Pat.fresh(Wild))
  };

/* Helper: check if an Atom matches the expected selector string.
   Handles float normalization: "3.14" matches Float(3.14) even though
   Atom.to_literal gives "3.140000". */
let atom_matches_str = (atom: Atom.t, expected: string): bool =>
  if (Atom.to_literal(atom) == expected) {
    true;
  } else {
    /* Try numeric comparison for floats */
    switch (atom) {
    | Float(f) =>
      switch (float_of_string_opt(expected)) {
      | Some(ef) => f == ef
      | None => false
      }
    | _ => false
    };
  };

/* Match a binary operator expression against an operator string.
   Returns Some((left, right)) if the expression is a BinOp with matching
   operator or a Cons with "::". Returns None otherwise. */
let match_binop = (op: string, e: Exp.t): option((Exp.t, Exp.t)) =>
  switch (Exp.term_of(e)) {
  | BinOp(bin_op, e1, e2) when Operators.bin_op_to_string(bin_op) == op =>
    Some((e1, e2))
  | Cons(e1, e2) when op == "::" => Some((e1, e2))
  | _ => None
  };

/* Find a binder by name in an expression, returning (binder_exp, def, body).
   Searches through nested let/type/module chains. */
let rec find_binder_in_exp =
        (name: string, e: Exp.t): option((Exp.t, Exp.t)) =>
  switch (Exp.term_of(e)) {
  | Let(pat, def, body) =>
    switch (pat_name(pat)) {
    | Some(n) when String.equal(n, name) => Some((def, body))
    | _ => find_binder_in_exp(name, body)
    }
  | TyAlias(tpat, _tdef, body) =>
    switch (tpat_name(tpat)) {
    | Some(n) when String.equal(n, name) =>
      /* For type aliases, the "def" isn't an Exp, so we skip into body */
      find_binder_in_exp(name, body)
    | _ => find_binder_in_exp(name, body)
    }
  | ModuleExp(mpat, def, body) =>
    switch (mpat_name(mpat)) {
    | Some(n) when String.equal(n, name) => Some((def, body))
    | _ => find_binder_in_exp(name, body)
    }
  | Module(items) =>
    /* Search module items for a let/type/module named `name` */
    List.fold_left(
      (acc, item: Mod.t) =>
        switch (acc) {
        | Some(_) => acc
        | None =>
          switch (item.term) {
          | ModLet(pat, def) =>
            switch (pat_name(pat)) {
            | Some(n) when String.equal(n, name) =>
              /* For module let, def is the definition. No body per se. */
              Some((def, e))
            | _ => None
            }
          | ModuleMod(mpat, def) =>
            switch (mpat_name(mpat)) {
            | Some(n) when String.equal(n, name) => Some((def, e))
            | _ => None
            }
          | _ => None
          }
        },
      None,
      items,
    )
  | Parens(inner) => find_binder_in_exp(name, inner)
  | _ => None
  };

/* Find a Mod.t item by name inside Module(items), walking through
   Let/ModuleExp/TyAlias chains to reach the module. Returns the
   Mod.t item itself (ModLet, ModuleMod, or ModType) so callers
   can return FocusMod. */
let rec find_mod_item_by_name = (name: string, e: Exp.t): option(Mod.t) =>
  switch (Exp.term_of(e)) {
  | Let(_, _, body) => find_mod_item_by_name(name, body)
  | ModuleExp(_, _, body) => find_mod_item_by_name(name, body)
  | TyAlias(_, _, body) => find_mod_item_by_name(name, body)
  | Module(items) =>
    List.fold_left(
      (acc, item: Mod.t) =>
        switch (acc) {
        | Some(_) => acc
        | None =>
          switch (item.term) {
          | ModLet(pat, _) =>
            switch (pat_name(pat)) {
            | Some(n) when String.equal(n, name) => Some(item)
            | _ => None
            }
          | ModuleMod(mpat, _) =>
            switch (mpat_name(mpat)) {
            | Some(n) when String.equal(n, name) => Some(item)
            | _ => None
            }
          | ModType(tpat, _) =>
            switch (tpat_name(tpat)) {
            | Some(n) when String.equal(n, name) => Some(item)
            | _ => None
            }
          | _ => None
          }
        },
      None,
      items,
    )
  | _ => None
  };

/* Find all binders with a given name in an expression chain.
   Returns list of (def, body) pairs in order of appearance (0-indexed). */
let rec find_all_binders_named =
        (name: string, e: Exp.t): list((Exp.t, Exp.t)) =>
  switch (Exp.term_of(e)) {
  | Let(pat, def, body) =>
    let here =
      switch (pat_name(pat)) {
      | Some(n) when String.equal(n, name) => [(def, body)]
      | _ => []
      };
    here @ find_all_binders_named(name, body);
  | ModuleExp(mpat, def, body) =>
    let here =
      switch (mpat_name(mpat)) {
      | Some(n) when String.equal(n, name) => [(def, body)]
      | _ => []
      };
    here @ find_all_binders_named(name, body);
  | TyAlias(_tpat, _tdef, body) => find_all_binders_named(name, body)
  | Parens(inner) => find_all_binders_named(name, inner)
  | Module(items) =>
    List.concat_map(
      (item: Mod.t) =>
        switch (item.term) {
        | ModLet(pat, def) =>
          switch (pat_name(pat)) {
          | Some(n) when String.equal(n, name) => [(def, e)]
          | _ => []
          }
        | ModuleMod(mpat, def) =>
          switch (mpat_name(mpat)) {
          | Some(n) when String.equal(n, name) => [(def, e)]
          | _ => []
          }
        | _ => []
        },
      items,
    )
  | _ => []
  };

/* Find the nth (0-indexed) binder with the given name */
let find_binder_indexed =
    (name: string, idx: int, e: Exp.t): option((Exp.t, Exp.t)) => {
  let all = find_all_binders_named(name, e);
  List.nth_opt(all, idx);
};

/* Find the Let/ModuleExp node for a given binder name, at a specific index */
let find_let_node_indexed = (name: string, idx: int, e: Exp.t): option(Exp.t) => {
  let rec collect = (e: Exp.t): list(Exp.t) =>
    switch (Exp.term_of(e)) {
    | Let(pat, _def, body) =>
      let here =
        switch (pat_name(pat)) {
        | Some(n) when String.equal(n, name) => [e]
        | _ => []
        };
      here @ collect(body);
    | ModuleExp(mpat, _def, body) =>
      let here =
        switch (mpat_name(mpat)) {
        | Some(n) when String.equal(n, name) => [e]
        | _ => []
        };
      here @ collect(body);
    | TyAlias(tpat, _tdef, body) =>
      let here =
        switch (tpat_name(tpat)) {
        | Some(n) when String.equal(n, name) => [e]
        | _ => []
        };
      here @ collect(body);
    | _ => []
    };
  List.nth_opt(collect(e), idx);
};

/* Collect all binder names visible at the top level of an expression.
   Walks through let/type/module chains and module item lists. */
let rec collect_binder_names = (e: Exp.t): list(string) =>
  switch (Exp.term_of(e)) {
  | Let(pat, _def, body) =>
    let here =
      switch (pat_name(pat)) {
      | Some(n) => [n]
      | None => []
      };
    here @ collect_binder_names(body);
  | TyAlias(tpat, _tdef, body) =>
    let here =
      switch (tpat_name(tpat)) {
      | Some(n) => [n]
      | None => []
      };
    here @ collect_binder_names(body);
  | ModuleExp(mpat, _def, body) =>
    let here =
      switch (mpat_name(mpat)) {
      | Some(n) => [n]
      | None => []
      };
    here @ collect_binder_names(body);
  | Module(items) =>
    List.concat_map(
      (item: Mod.t) =>
        switch (item.term) {
        | ModLet(pat, _) =>
          switch (pat_name(pat)) {
          | Some(n) => [n]
          | None => []
          }
        | ModuleMod(mpat, _) =>
          switch (mpat_name(mpat)) {
          | Some(n) => [n]
          | None => []
          }
        | ModType(tpat, _) =>
          switch (tpat_name(tpat)) {
          | Some(n) => [n]
          | None => []
          }
        | _ => []
        },
      items,
    )
  | _ => []
  };

/* Levenshtein edit distance between two strings */
let levenshtein = (s: string, t: string): int => {
  let m = String.length(s);
  let n = String.length(t);
  if (m == 0) {
    n;
  } else if (n == 0) {
    m;
  } else {
    let d = Array.make_matrix(m + 1, n + 1, 0);
    for (i in 0 to m) {
      d[i][0] = i;
    };
    for (j in 0 to n) {
      d[0][j] = j;
    };
    for (i in 1 to m) {
      for (j in 1 to n) {
        let cost =
          if (Char.equal(s.[i - 1], t.[j - 1])) {
            0;
          } else {
            1;
          };
        d[i][j] =
          min(d[i - 1][j] + 1, min(d[i][j - 1] + 1, d[i - 1][j - 1] + cost));
      };
    };
    d[m][n];
  };
};

/* Suggest similar binder names for a failed name lookup */
let suggest_similar_names =
    (target: string, available: list(string)): option(string) => {
  let candidates =
    available
    |> List.map(name => (name, levenshtein(target, name)))
    |> List.filter(((_, dist)) => dist <= 2 && dist > 0)
    |> List.sort(((_, d1), (_, d2)) => Int.compare(d1, d2));
  switch (candidates) {
  | [(name, _), ..._] => Some(name)
  | [] => None
  };
};

/* The spine of a let expression: pat, def, body */
type let_spine = {
  pat: Pat.t,
  def: Exp.t,
  body: Exp.t,
  whole: Exp.t,
};

/* The spine of an if expression: cond, then_, else_ */
type if_spine = {
  cond: Exp.t,
  then_: Exp.t,
  else_: Exp.t,
  whole: Exp.t,
};

/* The spine of a case arm: pat, body */
type case_arm = {
  pat: Pat.t,
  body: Exp.t,
};

/* The spine of a fun expression: pat, body */
type fun_spine = {
  pat: Pat.t,
  body: Exp.t,
  whole: Exp.t,
};

/* The spine of a test expression: body */
type test_spine = {
  body: Exp.t,
  whole: Exp.t,
};

/* === Child indexing === */

/* Helper: select from a fixed list of focus_target children */
let nth_of = (n: int, children: list(focus_target)): option(focus_target) =>
  List.nth_opt(children, n);

/* Get the nth structural child of an expression.
   Children are numbered left-to-right as in source syntax.
   Metadata fields (env, direction, provenance) are skipped.
   Match rules are virtual pairs handled by walk's ChildIndex case. */
let nth_child_exp = (n: int, e: Exp.t): option(focus_target) => {
  let exp = e' => FocusExp(e');
  let pat = p => FocusPat(p);
  let typ = t => FocusTyp(t);
  let mod_ = m => FocusMod(m);
  switch (Exp.term_of(e)) {
  /* 0 children */
  | Invalid(_)
  | EmptyHole
  | Deferral(_)
  | Undefined
  | Atom(_)
  | Constructor(_, _)
  | Var(_)
  | BuiltinFun(_)
  | Label(_)
  | ExplicitNonlabel
  | LivelitName(_)
  | MultiHole(_) => None

  /* 1 Exp child */
  | DynamicErrorHole(e1, _)
  | UnOp(_, e1)
  | Test(e1)
  | Parens(e1)
  | Projector(_, e1)
  | Closure(_, e1)
  | ProofObject(e1)
  | Filter(_, e1)
  | TypFun(_, e1, _) => nth_of(n, [exp(e1)])

  /* 2 Exp children */
  | BinOp(_, e1, e2)
  | Seq(e1, e2)
  | Cons(e1, e2)
  | ListConcat(e1, e2)
  | Dot(e1, e2)
  | TupLabel(e1, e2)
  | TupleExtension(e1, e2)
  | HintedTest(e1, e2)
  | Ap(_, e1, e2) => nth_of(n, [exp(e1), exp(e2)])

  /* Pat, Exp */
  | Fun(p, body, _, _)
  | FixF(p, body, _)
  | Forall(p, body) => nth_of(n, [pat(p), exp(body)])

  /* Exp, Typ */
  | Asc(e1, t)
  | TypAp(e1, t) => nth_of(n, [exp(e1), typ(t)])
  /* Typ, Exp */
  | Use(t, body) => nth_of(n, [typ(t), exp(body)])

  /* Pat, Exp, Exp */
  | Let(p, def, body)
  | Theorem(p, def, body) => nth_of(n, [pat(p), exp(def), exp(body)])
  /* 3 Exp children */
  | If(e1, e2, e3) => nth_of(n, [exp(e1), exp(e2), exp(e3)])
  /* Typ, Exp (TPat/MPat skipped) */
  | TyAlias(_, t, body) => nth_of(n, [typ(t), exp(body)])
  | ModuleExp(_, def, body) => nth_of(n, [exp(def), exp(body)])

  /* Variable-length */
  | Tuple(items)
  | ListLit(items) => List.nth_opt(items, n) |> Option.map(exp)
  | DeferredAp(fn, args) => nth_of(n, [exp(fn), ...List.map(exp, args)])
  /* Match: #0=scrut. Rule pairs are virtual nodes handled in walk. */
  | Match(scrut, _) => nth_of(n, [exp(scrut)])
  /* Module items */
  | Module(items) => List.nth_opt(items, n) |> Option.map(mod_)
  };
};

/* Get the nth child of a pattern */
let nth_child_pat = (n: int, p: Pat.t): option(focus_target) => {
  let pat = p => FocusPat(p);
  let typ = t => FocusTyp(t);
  switch (Pat.term_of(p)) {
  | Invalid(_)
  | EmptyHole
  | Wild
  | Atom(_)
  | Constructor(_, _)
  | Var(_)
  | Label(_)
  | ExplicitNonlabel
  | MultiHole(_) => None
  | Parens(p1)
  | Projector(_, p1) => nth_of(n, [pat(p1)])
  | Cons(p1, p2)
  | TupLabel(p1, p2)
  | Ap(p1, p2) => nth_of(n, [pat(p1), pat(p2)])
  | Asc(p1, t) => nth_of(n, [pat(p1), typ(t)])
  | Tuple(items)
  | ListLit(items) => List.nth_opt(items, n) |> Option.map(pat)
  };
};

/* Get the nth child of a type */
let nth_child_typ = (n: int, t: Typ.t): option(focus_target) => {
  let typ = t => FocusTyp(t);
  let exp = e => FocusExp(e);
  switch (Typ.term_of(t)) {
  | Unknown(_)
  | Atom(_)
  | Var(_)
  | Label(_)
  | ExplicitNonlabel
  | Sum(_)
  | Sig(_) => None
  | List(t1)
  | Parens(t1)
  | Projector(_, t1)
  | Rec(_, t1)
  | Poly(_, t1) => nth_of(n, [typ(t1)])
  | Arrow(t1, t2)
  | TupLabel(t1, t2)
  | ProdProjection(t1, t2)
  | ProdExtension(t1, t2) => nth_of(n, [typ(t1), typ(t2)])
  | ProofOf(e) => nth_of(n, [exp(e)])
  | Prod(items) => List.nth_opt(items, n) |> Option.map(typ)
  };
};

/* Get the nth child of a module item */
let nth_child_mod = (n: int, m: Mod.t): option(focus_target) => {
  let exp = e => FocusExp(e);
  let pat = p => FocusPat(p);
  let typ = t => FocusTyp(t);
  switch (m.term) {
  | Invalid(_)
  | EmptyHole
  | MultiHole(_) => None
  | ModLet(p, def) => nth_of(n, [pat(p), exp(def)])
  | ModType(_, t) => nth_of(n, [typ(t)])
  | ModuleMod(_, def) => nth_of(n, [exp(def)])
  | ModExp(e) => nth_of(n, [exp(e)])
  };
};

/* Resolve an elaborated semantic selector against an expression */
let resolve_sem = (steps: sem_selector, root: Exp.t): list(match_result) => {
  /* Walk the selector steps against a current expression context.
     Returns list of (focused_exp) for each match. */
  let rec walk = (steps: sem_selector, current: Exp.t): list(match_result) =>
    switch (steps) {
    | [] => []

    /* Focus: return the current expression as match */
    | [MatchFocus] => [mk_exp(current)]

    /* Focus + binop: % op _ focuses the left operand */
    | [MatchFocus, MatchDelimiter(op), MatchSlot] when is_binop_token(op) =>
      switch (match_binop(op, current)) {
      | Some((e1, _e2)) => [mk_exp(~bc="% " ++ op ++ " _", e1)]
      | None => []
      }

    /* Focus + more steps: focus on whatever the remaining steps select */
    | [MatchFocus, ...rest] => walk(rest, current)

    /* MatchAtom: match a literal value at the current node.
       Compares the printed form of the node against the expected string. */
    | [MatchAtom(expected)] =>
      switch (Exp.term_of(current)) {
      | Atom(actual) when atom_matches_str(actual, expected) => [
          mk_exp(~bc=expected, current),
        ]
      | Var(name) when name == expected => [mk_exp(~bc=expected, current)]
      | Constructor(name, _) when name == expected => [
          mk_exp(~bc=expected, current),
        ]
      | _ => []
      }
    | [MatchAtom(expected), ...rest] =>
      switch (Exp.term_of(current)) {
      | Atom(actual) when atom_matches_str(actual, expected) =>
        walk(rest, current)
      | Var(name) when name == expected => walk(rest, current)
      | Constructor(name, _) when name == expected => walk(rest, current)
      | _ => []
      }

    /* EnterBinderDef: find binder by name, enter its definition.
       Tries all binders with that name (handles shadowed bindings). */
    | [EnterBinderDef(name), ...rest] =>
      find_all_binders_named(name, current)
      |> List.concat_map(((def, _body)) => walk(rest, def))

    /* MatchName: find a binder by name in the current expression */
    | [MatchName(name)] =>
      /* If this is the final step with no focus, match the whole binding */
      switch (find_let_node(name, current)) {
      | Some(let_exp) => [mk_exp(~bc=name, let_exp)]
      | None =>
        /* Check module items — return FocusMod for the whole item */
        switch (find_mod_item_by_name(name, current)) {
        | Some(item) => [mk_mod(~bc=name, item)]
        | None =>
          switch (find_binder_in_exp(name, current)) {
          | Some((def, _)) => [mk_exp(~bc=name, def)]
          | None => []
          }
        }
      }

    /* name = % : select the definition of all binders named `name` */
    | [MatchName(name), MatchDelimiter("="), MatchFocus] =>
      find_all_binders_named(name, current)
      |> List.map(((def, _body)) => mk_exp(~bc=name ++ " = ...", def))

    /* name = <more> : enter the definition of all binders named `name` */
    | [MatchName(name), MatchDelimiter("="), ...rest] =>
      find_all_binders_named(name, current)
      |> List.concat_map(((def, _body)) => walk(rest, def))

    /* name ... in % : select the body of all binders named `name` */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
      find_all_binders_named(name, current)
      |> List.map(((_def, body)) => mk_exp(~bc=name ++ " ... in ...", body))

    /* name ... in <more> : enter the body of all binders named `name` */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), ...rest] =>
      find_all_binders_named(name, current)
      |> List.concat_map(((_def, body)) => walk(rest, body))

    | [MatchName(name), ...rest] =>
      /* Name followed by other steps: find the let node, continue */
      switch (find_let_node(name, current)) {
      | Some(let_exp) => walk(rest, let_exp)
      | None =>
        /* For terminal focus, prefer FocusMod for module items */
        switch (rest, find_mod_item_by_name(name, current)) {
        | ([MatchFocus], Some(item)) => [mk_mod(~bc=name, item)]
        | _ =>
          switch (find_binder_in_exp(name, current)) {
          | Some((def, _)) => walk(rest, def)
          | None => []
          }
        }
      }

    /* MatchNameIndex: indexed disambiguation for shadowed bindings */
    | [MatchNameIndex(name, idx)] =>
      switch (find_let_node_indexed(name, idx, current)) {
      | Some(let_exp) => [
          mk_exp(~bc=name ++ "#" ++ string_of_int(idx), let_exp),
        ]
      | None =>
        switch (find_binder_indexed(name, idx, current)) {
        | Some((def, _)) => [
            mk_exp(~bc=name ++ "#" ++ string_of_int(idx), def),
          ]
        | None => []
        }
      }

    | [MatchNameIndex(name, idx), MatchDelimiter("="), MatchFocus] =>
      switch (find_binder_indexed(name, idx, current)) {
      | Some((def, _body)) => [
          mk_exp(~bc=name ++ "#" ++ string_of_int(idx) ++ " = ...", def),
        ]
      | None => []
      }

    | [MatchNameIndex(name, idx), MatchDelimiter("="), ...rest] =>
      switch (find_binder_indexed(name, idx, current)) {
      | Some((def, _body)) => walk(rest, def)
      | None => []
      }

    | [
        MatchNameIndex(name, idx),
        MatchEllipsis,
        MatchKeyword("in"),
        MatchFocus,
      ] =>
      switch (find_binder_indexed(name, idx, current)) {
      | Some((_def, body)) => [
          mk_exp(
            ~bc=name ++ "#" ++ string_of_int(idx) ++ " ... in ...",
            body,
          ),
        ]
      | None => []
      }

    | [MatchNameIndex(name, idx), MatchEllipsis, MatchKeyword("in"), ...rest] =>
      switch (find_binder_indexed(name, idx, current)) {
      | Some((_def, body)) => walk(rest, body)
      | None => []
      }

    | [MatchNameIndex(name, idx), ...rest] =>
      switch (find_let_node_indexed(name, idx, current)) {
      | Some(let_exp) => walk(rest, let_exp)
      | None =>
        switch (find_binder_indexed(name, idx, current)) {
        | Some((def, _)) => walk(rest, def)
        | None => []
        }
      }

    /* let keyword: try all lets in the chain */
    | [MatchKeyword("let"), MatchNameIndex(name, idx), ...after_name] =>
      /* Indexed: find the nth let spine with matching name.
         Same pattern used for module#N and type#N below. */
      let matching_spines =
        find_all_lets(current)
        |> List.filter((spine: let_spine) =>
             Option.equal(String.equal, pat_name(spine.pat), Some(name))
           );
      switch (List.nth_opt(matching_spines, idx)) {
      | Some(spine) =>
        walk_let_spine(spine, [MatchName(name), ...after_name])
      | None => []
      };
    | [MatchKeyword("let"), ...rest] =>
      find_all_lets(current)
      |> List.concat_map(spine => walk_let_spine(spine, rest))

    /* if keyword: expect current to be an If */
    | [MatchKeyword("if"), ...rest] =>
      switch (Exp.term_of(current)) {
      | If(cond, then_, else_) =>
        walk_if_spine(
          {
            cond,
            then_,
            else_,
            whole: current,
          },
          rest,
        )
      | _ => []
      }

    /* case keyword: expect current to be a Match */
    | [MatchKeyword("case"), ...rest] =>
      switch (Exp.term_of(current)) {
      | Match(scrut, rules) => walk_case_spine(current, scrut, rules, rest)
      | _ => []
      }

    /* module keyword: indexed disambiguation for shadowed module binders */
    | [MatchKeyword("module"), MatchNameIndex(name, idx), ...after_name] =>
      let matching =
        find_all_modules(current)
        |> List.filter(((name_opt, _, _, _, _)) =>
             Option.equal(String.equal, name_opt, Some(name))
           );
      switch (List.nth_opt(matching, idx)) {
      | Some((name_opt, def, whole, body_opt, mod_item_opt)) =>
        walk_after_module_kw(
          name_opt,
          def,
          whole,
          body_opt,
          mod_item_opt,
          [MatchName(name), ...after_name],
        )
      | None => []
      };

    /* module keyword */
    | [MatchKeyword("module"), ...rest] =>
      switch (Exp.term_of(current)) {
      | ModuleExp(mpat, def, body) =>
        switch (mpat_name(mpat)) {
        | Some(_name) =>
          walk_after_module_kw(
            mpat_name(mpat),
            def,
            current,
            Some(body),
            None,
            rest,
          )
        | None => []
        }
      | Let(pat, def, body) =>
        switch (Exp.term_of(def)) {
        | Module(_) =>
          switch (pat_name(pat)) {
          | Some(_) =>
            walk_after_module_kw(
              pat_name(pat),
              def,
              current,
              Some(body),
              None,
              rest,
            )
          | None => []
          }
        | _ => []
        }
      | Module(items) =>
        /* Match ModuleMod items inside a module body */
        List.concat_map(
          (item: Mod.t) =>
            switch (item.term) {
            | ModuleMod(mpat, def) =>
              switch (mpat_name(mpat)) {
              | Some(_) =>
                walk_after_module_kw(
                  mpat_name(mpat),
                  def,
                  current,
                  None,
                  Some(item),
                  rest,
                )
              | None => []
              }
            | _ => []
            },
          items,
        )
      | _ => []
      }

    /* type keyword: indexed disambiguation for shadowed type binders */
    | [MatchKeyword("type"), MatchNameIndex(name, idx), ...after_name] =>
      let matching =
        find_all_types(current)
        |> List.filter(((tpat, _, _, _, _)) =>
             Option.equal(String.equal, tpat_name(tpat), Some(name))
           );
      switch (List.nth_opt(matching, idx)) {
      | Some((tpat, whole, body_opt, tdef_opt, mod_item_opt)) =>
        walk_after_type_kw(
          tpat,
          whole,
          body_opt,
          tdef_opt,
          mod_item_opt,
          [MatchName(name), ...after_name],
        )
      | None => []
      };

    /* type keyword */
    | [MatchKeyword("type"), ...rest] =>
      switch (Exp.term_of(current)) {
      | TyAlias(tpat, tdef, body) =>
        walk_after_type_kw(
          tpat,
          current,
          Some(body),
          Some(tdef),
          None,
          rest,
        )
      | Module(items) =>
        /* Match ModType items inside a module body */
        List.concat_map(
          (item: Mod.t) =>
            switch (item.term) {
            | ModType(tpat, tdef) =>
              walk_after_type_kw(
                tpat,
                current,
                None,
                Some(tdef),
                Some(item),
                rest,
              )
            | _ => []
            },
          items,
        )
      | _ => []
      }

    /* fun keyword */
    | [MatchKeyword("fun"), ...rest] =>
      switch (Exp.term_of(current)) {
      | Fun(pat, body, _, _) =>
        walk_fun_spine(
          {
            pat,
            body,
            whole: current,
          },
          rest,
        )
      | _ => []
      }

    /* test keyword */
    | [MatchKeyword("test"), ...rest] =>
      switch (Exp.term_of(current)) {
      | Test(body) =>
        walk_test_spine(
          {
            body,
            whole: current,
          },
          rest,
        )
      | _ => []
      }

    /* List literal [ ... ] */
    | [MatchDelimiter("["), ...rest] =>
      switch (Exp.term_of(current)) {
      | ListLit(items) => walk_list_spine(items, current, rest)
      | _ => []
      }

    /* Tuple ( ... ) — only matches actual Tuple nodes.
       Parens(Tuple(...)) is transparent: descend_all will reach the Tuple. */
    | [MatchDelimiter("("), ...rest] =>
      switch (Exp.term_of(current)) {
      | Tuple(items) => walk_seq_spine(items, current, rest)
      | _ => []
      }

    /* Module body { ... ; ... } */
    | [MatchDelimiter("{"), ...rest] =>
      switch (Exp.term_of(current)) {
      | Module(items) => walk_mod_spine(items, current, rest)
      | _ => []
      }

    /* Pipe: match a case arm with the given constructor */
    | [MatchDelimiter("|"), ...rest] => walk_pipe(current, rest)

    /* BinOp: pattern with operator delimiter matches a binary operation.
       Handles full left-op-right patterns to correctly focus either side.
       NOTE: / (division) is not supported as an operator
       due to conflict with chain (/) syntax. */

    /* _ op % : focus right operand */
    | [MatchSlot, MatchDelimiter(op), MatchFocus] when is_binop_token(op) =>
      switch (match_binop(op, current)) {
      | Some((_e1, e2)) => [mk_exp(~bc="_ " ++ op ++ " %", e2)]
      | None => []
      }

    /* _ op _ : focus whole BinOp (with or without trailing implicit focus) */
    | [MatchSlot, MatchDelimiter(op), MatchSlot]
    | [MatchSlot, MatchDelimiter(op), MatchSlot, MatchFocus]
        when is_binop_token(op) =>
      switch (match_binop(op, current)) {
      | Some(_) => [mk_exp(~bc="_ " ++ op ++ " _", current)]
      | None => []
      }

    /* _ op <more> : descend into right operand */
    | [MatchSlot, MatchDelimiter(op), ...rest] when is_binop_token(op) =>
      switch (match_binop(op, current)) {
      | Some((_e1, e2)) => walk(rest, e2)
      | None => []
      }

    /* bare op <rest> : operator without explicit left slot */
    | [MatchDelimiter(op), ...rest] when is_binop_token(op) =>
      switch (match_binop(op, current)) {
      | Some((e1, e2)) => walk_binop_spine(e1, e2, current, rest)
      | None => []
      }

    /* DescendInto: search all descendants */
    | [DescendInto, ...rest] => descend_all(current, rest)

    /* Ellipsis: skip slots */
    | [MatchEllipsis, ...rest] =>
      /* Ellipsis consumes zero or more spine positions,
         so we try matching the rest at the current position
         and also skip forward through spine items. */
      walk(rest, current)

    | [MatchSlot, ...rest] =>
      /* Slot: skip one position. In most contexts this is
         consumed by spine walkers. For top-level, skip. */
      walk(rest, current)

    /* ChildIndex: descend into the nth structural child */
    | [ChildIndex(n), ...rest] =>
      /* Special case: Match rule pairs. #N (N>=1) enters rule N-1,
         then the next ChildIndex picks pat (#0) or body (#1). */
      switch (Exp.term_of(current)) {
      | Match(_, rules) when n >= 1 =>
        switch (List.nth_opt(rules, n - 1)) {
        | Some((pat, body)) =>
          switch (rest) {
          | [ChildIndex(0), ...rest2] => walk_pat(rest2, pat)
          | [ChildIndex(1), ...rest2] => walk(rest2, body)
          | [MatchFocus] => [
              mk_exp(~bc="rule " ++ string_of_int(n - 1), body),
            ]
          | [] => [mk_exp(~bc="rule " ++ string_of_int(n - 1), body)]
          | _ => []
          }
        | None => []
        }
      | _ =>
        switch (nth_child_exp(n, current)) {
        | Some(FocusExp(e)) => walk(rest, e)
        | Some(FocusPat(p)) => walk_pat(rest, p)
        | Some(FocusTyp(t)) => walk_typ(rest, t)
        | Some(FocusMod(m)) => walk_mod(rest, m)
        | None => []
        }
      }

    | _ => [] /* unhandled pattern */
    }

  /* Walk a let spine after "let" keyword */
  and walk_let_spine = (spine: let_spine, steps: sem_selector) =>
    switch (steps) {
    | [] => [mk_exp(spine.whole)]
    /* let % : focus on pattern (next syntactic term after let) */
    | [MatchFocus] => [mk_pat(~bc="let (pat)", spine.pat)]

    /* let % <name> = : focus on pattern named <name> */
    | [MatchFocus, MatchName(name), MatchDelimiter("=")]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        mk_pat(~bc="let " ++ name ++ " (pat)", spine.pat),
      ]
    | [MatchFocus, MatchName(name), MatchDelimiter("="), ...rest]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk_pat(rest, spine.pat)

    /* let % <name> : focus on pattern (terminal) */
    | [MatchFocus, MatchName(name)]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        mk_pat(~bc="let " ++ name ++ " (pat)", spine.pat),
      ]

    /* let % = : slot-focus, pattern of any let */
    | [MatchFocus, MatchDelimiter("=")] => [
        mk_pat(~bc="let (pat)", spine.pat),
      ]
    | [MatchFocus, MatchDelimiter("="), ...rest] => walk_pat(rest, spine.pat)

    /* let <name> = % : focus on definition */
    | [MatchName(name), MatchDelimiter("="), MatchFocus]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        mk_exp(~bc="let " ++ name ++ " = ...", spine.def),
      ]

    /* let <name> = % ... : focus on definition, continue */
    | [MatchName(name), MatchDelimiter("="), MatchFocus, ...rest]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.def)

    /* let <name> ... in % : focus on body */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), MatchFocus]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        mk_exp(~bc="let " ++ name ++ " ... in ...", spine.body),
      ]

    /* let <name> ... in % <more> : focus on body, continue */
    | [
        MatchName(name),
        MatchEllipsis,
        MatchKeyword("in"),
        MatchFocus,
        ...rest,
      ]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.body)

    /* let <name> = _ in % : skip def, focus on body */
    | [
        MatchName(name),
        MatchDelimiter("="),
        MatchSlot,
        MatchKeyword("in"),
        MatchFocus,
      ]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        mk_exp(~bc="let " ++ name ++ " = _ in ...", spine.body),
      ]

    /* let <name> = _ in <more> : skip def, continue in body */
    | [
        MatchName(name),
        MatchDelimiter("="),
        MatchSlot,
        MatchKeyword("in"),
        ...rest,
      ]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.body)

    /* let <name> = ... : match but continue through remaining steps */
    | [MatchName(name), MatchDelimiter("="), ...rest]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.def)

    /* let <name> ... in <more> : skip to body */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), ...rest]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.body)

    /* Colon patterns: let <name> : % selects type annotation.
       In Hazel, `let x : T = def in body` parses as Let(Asc(pat, typ), def, body). */

    /* let <name> : % : focus on the type annotation */
    | [MatchName(name), MatchDelimiter(":"), MatchFocus] =>
      switch (Pat.term_of(spine.pat)) {
      | Asc(inner_pat, ty)
          when Option.equal(String.equal, Pat.is_var(inner_pat), Some(name)) => [
          mk_typ(~bc="let " ++ name ++ " : ...", ty),
        ]
      | _ => []
      }

    /* let <name> : _ = % : skip type annotation, focus on def */
    | [
        MatchName(name),
        MatchDelimiter(":"),
        MatchSlot,
        MatchDelimiter("="),
        MatchFocus,
      ] =>
      switch (Pat.term_of(spine.pat)) {
      | Asc(inner_pat, _ty)
          when Option.equal(String.equal, Pat.is_var(inner_pat), Some(name)) => [
          mk_exp(~bc="let " ++ name ++ " : _ = ...", spine.def),
        ]
      /* Also handle non-Asc patterns — name : _ = % just skips to def */
      | _ when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
          mk_exp(~bc="let " ++ name ++ " : _ = ...", spine.def),
        ]
      | _ => []
      }

    /* let <name> : _ = _ ... in % : skip annotation and def, focus on body */
    | [
        MatchName(name),
        MatchDelimiter(":"),
        MatchSlot,
        MatchDelimiter("="),
        MatchSlot,
        MatchEllipsis,
        MatchKeyword("in"),
        MatchFocus,
      ] =>
      switch (Pat.term_of(spine.pat)) {
      | Asc(inner_pat, _ty)
          when Option.equal(String.equal, Pat.is_var(inner_pat), Some(name)) => [
          mk_exp(~bc="let " ++ name ++ " : _ = _ ... in ...", spine.body),
        ]
      | _ => []
      }

    /* let <name> : match name, return whole or continue */
    | [MatchName(name)]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        mk_exp(~bc="let " ++ name, spine.whole),
      ]
    | [MatchName(name), ...rest]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.whole)

    /* let _ = % : slot pattern, focus on def */
    | [MatchSlot, MatchDelimiter("="), MatchFocus] => [
        mk_exp(~bc="let _ = ...", spine.def),
      ]

    /* let _ = _ in % : slot pattern, skip def, focus on body */
    | [
        MatchSlot,
        MatchDelimiter("="),
        MatchSlot,
        MatchKeyword("in"),
        MatchFocus,
      ] => [
        mk_exp(~bc="let _ = _ in ...", spine.body),
      ]

    /* let _ = _ in <more> : slot pattern, skip def, continue in body */
    | [MatchSlot, MatchDelimiter("="), MatchSlot, MatchKeyword("in"), ...rest] =>
      walk(rest, spine.body)

    /* let _ ... in % : slot pattern, focus on body */
    | [MatchSlot, MatchEllipsis, MatchKeyword("in"), MatchFocus] => [
        mk_exp(~bc="let _ ... in ...", spine.body),
      ]

    | _ => []
    }

  /* Walk an if spine after "if" keyword */
  and walk_if_spine = (spine: if_spine, steps: sem_selector) =>
    switch (steps) {
    /* if % : focus on condition */
    | [MatchFocus] => [mk_exp(~bc="if ...", spine.cond)]

    /* if _ then % : focus on then branch */
    | [MatchSlot, MatchKeyword("then"), MatchFocus] => [
        mk_exp(~bc="if _ then ...", spine.then_),
      ]

    /* if ... else % : focus on else branch */
    | [MatchEllipsis, MatchKeyword("else"), MatchFocus] => [
        mk_exp(~bc="if ... else ...", spine.else_),
      ]

    /* if _ then _ else % : focus on else branch */
    | [
        MatchSlot,
        MatchKeyword("then"),
        MatchSlot,
        MatchKeyword("else"),
        MatchFocus,
      ] => [
        mk_exp(~bc="if _ then _ else ...", spine.else_),
      ]

    | _ => []
    }

  /* Walk a case spine after "case" keyword */
  and walk_case_spine =
      (
        whole: Exp.t,
        scrut: Exp.t,
        rules: list((Pat.t, Exp.t)),
        steps: sem_selector,
      ) =>
    switch (steps) {
    /* case % : focus on scrutinee */
    | [MatchFocus] => [mk_exp(~bc="case ...", scrut)]

    /* case ... | <ctor> => % : find arm by constructor, focus on body */
    | [MatchEllipsis, MatchDelimiter("|"), ...rest] =>
      walk_pipe_in_rules(rules, rest)

    /* case _ | <ctor> => % : skip scrutinee, find arm */
    | [MatchSlot, MatchDelimiter("|"), ...rest] =>
      walk_pipe_in_rules(rules, rest)

    | _ =>
      let _ = whole;
      [];
    }

  /* Walk pipe (|) in case arms */
  and walk_pipe = (current: Exp.t, steps: sem_selector) =>
    switch (Exp.term_of(current)) {
    | Match(_scrut, rules) => walk_pipe_in_rules(rules, steps)
    | _ => []
    }

  /* NOTE: Constructor matching convenience — when a selector names a constructor
     (e.g. `| A =>`), we match arms whose pattern *head* is that constructor,
     including applied constructors like A(x, y). This is a pragmatic shorthand:
     compositionally, `A` should only match nullary `A`, and `A(x, y)` would
     require something like `| A(_) =>`. We don't yet have that syntax, so we
     treat the name as "constructor head" for now. */
  and walk_pipe_in_rules =
      (rules: list((Pat.t, Exp.t)), steps: sem_selector) =>
    switch (steps) {
    /* | % <name> [=>] : focus on arm pattern */
    | [MatchFocus, MatchName(name), MatchDelimiter("=>")]
    | [MatchFocus, MatchName(name)] =>
      rules
      |> List.filter_map(((pat, _body)) => {
           let matches =
             switch (pat_name(pat)) {
             | Some(n) when String.equal(n, name) => true
             | _ =>
               switch (Pat.term_of(pat)) {
               | Ap({term: Constructor(cname, _), _}, _)
                   when String.equal(cname, name) =>
                 true
               | Constructor(cname, _) when String.equal(cname, name) => true
               | _ => false
               }
             };
           matches ? Some(mk_pat(~bc="| " ++ name ++ " (pat)", pat)) : None;
         })

    /* | % <name> => <more> : focus on arm pattern, continue into it */
    | [MatchFocus, MatchName(name), MatchDelimiter("=>"), ...rest] =>
      rules
      |> List.concat_map(((pat, _body)) => {
           let matches =
             switch (pat_name(pat)) {
             | Some(n) when String.equal(n, name) => true
             | _ =>
               switch (Pat.term_of(pat)) {
               | Ap({term: Constructor(cname, _), _}, _)
                   when String.equal(cname, name) =>
                 true
               | Constructor(cname, _) when String.equal(cname, name) => true
               | _ => false
               }
             };
           if (matches) {
             walk_pat(rest, pat);
           } else {
             [];
           };
         })

    /* | % => : slot-focus, pattern of each arm */
    | [MatchFocus, MatchDelimiter("=>")] =>
      rules |> List.map(((pat, _body)) => mk_pat(~bc="| (pat)", pat))

    /* | % => <more> : slot-focus, continue into each arm pattern */
    | [MatchFocus, MatchDelimiter("=>"), ...rest] =>
      rules |> List.concat_map(((pat, _body)) => walk_pat(rest, pat))

    /* | <name> => % : find arm by constructor name */
    | [MatchName(name), MatchDelimiter("=>"), MatchFocus] =>
      rules
      |> List.filter_map(((pat, body)) =>
           switch (pat_name(pat)) {
           | Some(n) when String.equal(n, name) =>
             Some(mk_exp(~bc="| " ++ name ++ " => ...", body))
           | _ =>
             /* Also check for constructor patterns like Foo(x) */
             switch (Pat.term_of(pat)) {
             | Ap({term: Constructor(cname, _), _}, _)
                 when String.equal(cname, name) =>
               Some(mk_exp(~bc="| " ++ name ++ "(...) => ...", body))
             | Constructor(cname, _) when String.equal(cname, name) =>
               Some(mk_exp(~bc="| " ++ name ++ " => ...", body))
             | _ => None
             }
           }
         )

    /* | <name> => <more steps> : find arm, continue */
    | [MatchName(name), MatchDelimiter("=>"), ...rest] =>
      rules
      |> List.concat_map(((pat, body)) => {
           let matches =
             switch (pat_name(pat)) {
             | Some(n) when String.equal(n, name) => true
             | _ =>
               switch (Pat.term_of(pat)) {
               | Ap({term: Constructor(cname, _), _}, _)
                   when String.equal(cname, name) =>
                 true
               | Constructor(cname, _) when String.equal(cname, name) => true
               | _ => false
               }
             };
           if (matches) {
             walk(rest, body);
           } else {
             [];
           };
         })

    /* | _ => % : wildcard, match any single arm body */
    | [MatchSlot, MatchDelimiter("=>"), MatchFocus] =>
      rules
      |> List.map(((pat, body)) => {
           let name =
             switch (pat_name(pat)) {
             | Some(n) => n
             | None => "_"
             };
           mk_exp(~bc="| " ++ name ++ " => ...", body);
         })

    /* | _ => <more steps> : wildcard, continue into each arm body */
    | [MatchSlot, MatchDelimiter("=>"), ...rest] =>
      rules |> List.concat_map(((_pat, body)) => walk(rest, body))

    /* | _... <more> : skip zero or more arms, try rest at each position */
    | [MatchEllipsis, ...rest] =>
      let rec try_from = remaining_rules =>
        switch (remaining_rules) {
        | [] => []
        | [_, ...tail] =>
          let here = walk_pipe_in_rules(remaining_rules, rest);
          here @ try_from(tail);
        };
      let all = try_from(rules);
      /* Deduplicate by focused_id (ellipsis can reach same arm from
         multiple starting positions) */
      let seen = Hashtbl.create(List.length(all));
      List.filter(
        ({focused_id, _}) =>
          if (Hashtbl.mem(seen, focused_id)) {
            false;
          } else {
            Hashtbl.add(seen, focused_id, ());
            true;
          },
        all,
      );

    | _ => []
    }

  /* Walk after "module" keyword.
     body_opt is passed explicitly so this works for both top-level
     ModuleExp/Let (which have bodies) and ModuleMod inside Module
     items (which don't — body_opt=None).
     mod_item_opt: when matching inside Module(items), the Mod.t item
     is passed so bare-name match can return FocusMod. */
  and walk_after_module_kw =
      (
        name_opt: option(string),
        def: Exp.t,
        whole: Exp.t,
        body_opt: option(Exp.t),
        mod_item_opt: option(Mod.t),
        steps: sem_selector,
      ) => {
    let name_str = Option.value(~default="_", name_opt);
    let name_matches = n => Option.equal(String.equal, name_opt, Some(n));
    switch (steps) {
    /* module M : bare name match. For module items, return FocusMod;
       for top-level module expressions, return FocusExp(whole). */
    | [MatchName(name)] when name_matches(name) =>
      switch (mod_item_opt) {
      | Some(item) => [mk_mod(~bc="module " ++ name, item)]
      | None => [mk_exp(~bc="module " ++ name, whole)]
      }

    /* module M = % : focus on module def */
    | [MatchName(name), MatchDelimiter("="), MatchFocus]
        when name_matches(name) => [
        mk_exp(~bc="module " ++ name ++ " = ...", def),
      ]

    /* module M = ... : match by name, continue into def */
    | [MatchName(name), MatchDelimiter("="), ...rest]
        when name_matches(name) =>
      walk(rest, def)

    /* module M _... in % : skip def, focus on body */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), MatchFocus]
        when name_matches(name) =>
      switch (body_opt) {
      | Some(body) => [mk_exp(~bc="module " ++ name ++ " ... in ...", body)]
      | None => []
      }

    /* module M _... in <more> : skip def, continue in body */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), ...rest]
        when name_matches(name) =>
      switch (body_opt) {
      | Some(body) => walk(rest, body)
      | None => []
      }

    /* module M <more> : match by name, continue.
       If rest is just [MatchFocus] and we have a mod item, return FocusMod.
       Otherwise continue walking. */
    | [MatchName(name), MatchFocus] when name_matches(name) =>
      switch (mod_item_opt) {
      | Some(item) => [mk_mod(~bc="module " ++ name, item)]
      | None => [mk_exp(~bc="module " ++ name, whole)]
      }
    | [MatchName(name), ...rest] when name_matches(name) =>
      switch (body_opt) {
      | Some(body) => walk(rest, body)
      | None => walk(rest, def)
      }

    /* module _ = % : wildcard name, focus on def */
    | [MatchSlot, MatchDelimiter("="), MatchFocus] => [
        mk_exp(~bc="module " ++ name_str ++ " = ...", def),
      ]

    /* module _ = <more> : wildcard name, continue into def */
    | [MatchSlot, MatchDelimiter("="), ...rest] => walk(rest, def)

    /* module _... in % : skip name and def, focus on body */
    | [MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
      switch (body_opt) {
      | Some(body) => [
          mk_exp(~bc="module " ++ name_str ++ " ... in ...", body),
        ]
      | None => []
      }

    /* module _... in <more> : skip to body, continue */
    | [MatchEllipsis, MatchKeyword("in"), ...rest] =>
      switch (body_opt) {
      | Some(body) => walk(rest, body)
      | None => []
      }

    /* module % : focus on whole module expression */
    | [MatchFocus] => [mk_exp(~bc="module " ++ name_str, whole)]

    /* module % <name> : implicit focus before name, return whole (fallback) */
    | [MatchFocus, MatchName(name)] when name_matches(name) =>
      switch (mod_item_opt) {
      | Some(item) => [mk_mod(~bc="module " ++ name, item)]
      | None => [mk_exp(~bc="module " ++ name, whole)]
      }

    | _ => []
    };
  }

  /* Walk after "type" keyword.
     type T = <typedef> in <body>
     body_opt and tdef_opt are passed explicitly so this works for both
     top-level TyAlias (which has body) and ModType inside Module items
     (which has no body — body_opt=None). */
  and walk_after_type_kw =
      (
        tpat: TPat.t,
        whole: Exp.t,
        body_opt: option(Exp.t),
        tdef_opt: option(Typ.t),
        mod_item_opt: option(Mod.t),
        steps: sem_selector,
      ) => {
    let name_opt = tpat_name(tpat);
    let name_matches = n => Option.equal(String.equal, name_opt, Some(n));
    let name_str = Option.value(~default="_", name_opt);
    /* Helper: handle steps after the name has been matched/consumed */
    let walk_after_name = (remaining: sem_selector) =>
      switch (remaining) {
      /* type T = % : focus on type definition */
      | [MatchDelimiter("="), MatchFocus] =>
        switch (tdef_opt) {
        | Some(tdef) => [mk_typ(~bc="type " ++ name_str ++ " = ...", tdef)]
        | None => []
        }
      /* type T _... in % : skip def, focus on body */
      | [MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
        switch (body_opt) {
        | Some(body) => [
            mk_exp(~bc="type " ++ name_str ++ " ... in ...", body),
          ]
        | None => []
        }
      /* type T _... in <more> : skip def, continue in body */
      | [MatchEllipsis, MatchKeyword("in"), ...rest] =>
        switch (body_opt) {
        | Some(body) => walk(rest, body)
        | None => []
        }
      | other => walk(other, whole)
      };
    /* For bare-name match, return FocusMod when inside a module item */
    let focus_whole_or_mod = () =>
      switch (mod_item_opt) {
      | Some(item) => [mk_mod(~bc="type " ++ name_str, item)]
      | None => [mk_exp(~bc="type " ++ name_str, whole)]
      };
    switch (steps) {
    /* type T : bare name match */
    | [MatchName(name)] when name_matches(name) => focus_whole_or_mod()
    /* type T % : bare name + implicit percent */
    | [MatchName(name), MatchFocus] when name_matches(name) =>
      focus_whole_or_mod()

    /* type T <more> : match by name, continue */
    | [MatchName(name), ...rest] when name_matches(name) =>
      walk_after_name(rest)

    /* type _ <more> : wildcard name */
    | [MatchSlot, ...rest] => walk_after_name(rest)

    /* type _... in % : skip name and def */
    | [MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
      switch (body_opt) {
      | Some(body) => [
          mk_exp(~bc="type " ++ name_str ++ " ... in ...", body),
        ]
      | None => []
      }

    | [MatchEllipsis, MatchKeyword("in"), ...rest] =>
      switch (body_opt) {
      | Some(body) => walk(rest, body)
      | None => []
      }

    /* type % : focus on whole type alias expression */
    | [MatchFocus] => [mk_exp(~bc="type " ++ name_str, whole)]

    /* type % <name> : implicit focus before name, return whole (fallback) */
    | [MatchFocus, MatchName(name)] when name_matches(name) =>
      focus_whole_or_mod()

    | _ => []
    };
  }

  /* Walk a fun spine after "fun" keyword */
  and walk_fun_spine = (spine: fun_spine, steps: sem_selector) =>
    switch (steps) {
    /* fun % : focus on parameter pattern (next syntactic term after fun) */
    | [MatchFocus] => [mk_pat(~bc="fun (pat)", spine.pat)]

    /* fun % <name> -> : focus on parameter pattern */
    | [MatchFocus, MatchName(name), MatchDelimiter("->")]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        mk_pat(~bc="fun " ++ name ++ " (pat)", spine.pat),
      ]
    | [MatchFocus, MatchName(name), MatchDelimiter("->"), ...rest]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk_pat(rest, spine.pat)

    /* fun % <name> : focus on parameter pattern (terminal) */
    | [MatchFocus, MatchName(name)]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        mk_pat(~bc="fun " ++ name ++ " (pat)", spine.pat),
      ]

    /* fun % -> : slot-focus, parameter of any fun */
    | [MatchFocus, MatchDelimiter("->")] => [
        mk_pat(~bc="fun (pat)", spine.pat),
      ]
    | [MatchFocus, MatchDelimiter("->"), ...rest] =>
      walk_pat(rest, spine.pat)

    /* fun _ -> % : skip pattern, focus on body */
    | [MatchSlot, MatchDelimiter("->"), MatchFocus] => [
        mk_exp(~bc="fun _ -> ...", spine.body),
      ]

    /* fun _ -> <more> : skip pattern, continue in body */
    | [MatchSlot, MatchDelimiter("->"), ...rest] => walk(rest, spine.body)

    /* fun ... -> % : skip pattern via ellipsis, focus on body */
    | [MatchEllipsis, MatchDelimiter("->"), MatchFocus] => [
        mk_exp(~bc="fun ... -> ...", spine.body),
      ]

    /* fun ... -> <more> : skip pattern via ellipsis, continue in body */
    | [MatchEllipsis, MatchDelimiter("->"), ...rest] =>
      walk(rest, spine.body)

    /* fun <name> -> % : match pattern by name, focus on body */
    | [MatchName(name), MatchDelimiter("->"), MatchFocus]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        mk_exp(~bc="fun " ++ name ++ " -> ...", spine.body),
      ]

    /* fun <name> -> <more> : match pattern by name, continue in body */
    | [MatchName(name), MatchDelimiter("->"), ...rest]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.body)

    | _ => []
    }

  /* Walk a test spine after "test" keyword */
  and walk_test_spine = (spine: test_spine, steps: sem_selector) =>
    switch (steps) {
    /* test % : focus on the test body */
    | [MatchFocus] => [mk_exp(~bc="test ...", spine.body)]

    /* test _ end [%] : match slot, then end keyword → whole test
       The MatchFocus variant handles the implicit focus rule. */
    | [MatchSlot, MatchKeyword("end")]
    | [MatchEllipsis, MatchKeyword("end")]
    | [MatchSlot, MatchKeyword("end"), MatchFocus]
    | [MatchEllipsis, MatchKeyword("end"), MatchFocus] => [
        mk_exp(~bc="test _ end", spine.whole),
      ]

    /* test _ % : match slot, then focus on body */
    | [MatchSlot, MatchFocus]
    | [MatchEllipsis, MatchFocus] => [mk_exp(~bc="test ...", spine.body)]

    /* test _ <more> : match slot, continue into body */
    | [MatchSlot, ...rest]
    | [MatchEllipsis, ...rest] => walk(rest, spine.body)

    /* test <more> : continue matching inside the body */
    | rest => walk(rest, spine.body)
    }

  /* BinOp spine walker: matches after the operator delimiter has been consumed.
     The remaining steps apply to the right operand. The left operand was
     consumed by MatchSlot/MatchFocus before the operator in the top-level walk. */
  and walk_binop_spine =
      (_left: Exp.t, right: Exp.t, whole: Exp.t, steps: sem_selector) =>
    switch (steps) {
    /* op % : focus right operand */
    | [MatchFocus] => [mk_exp(~bc="... op ...", right)]
    /* op _ : focus whole (slot consumes right, implicit star on whole) */
    | [MatchSlot]
    | [MatchSlot, MatchFocus]
    | [MatchEllipsis]
    | [MatchEllipsis, MatchFocus] => [mk_exp(~bc="_ op _", whole)]
    /* op <more> : descend into right operand */
    | rest => walk(rest, right)
    }

  /* Generic sequence spine walker: handles both list [e0, e1, ...]
     and tuple (e0, e1, ...) uniformly.
     Steps consume items via Slot (skip one), Ellipsis (skip many),
     Focus (return current item), or closing delimiters. */
  and walk_seq_spine =
      (items: list(Exp.t), whole: Exp.t, steps: sem_selector) => {
    /* Inner walk: consume steps against remaining items */
    let rec walk_items = (items: list(Exp.t), steps: sem_selector) =>
      switch (steps) {
      /* Focus: return current (first remaining) item */
      | [MatchFocus, ..._] =>
        switch (items) {
        | [item, ..._] => [mk_exp(item)]
        | [] => []
        }

      /* Ellipsis + Focus: skip to last, focus on it */
      | [MatchEllipsis, MatchFocus, ..._] =>
        switch (List.rev(items)) {
        | [last, ..._] => [mk_exp(last)]
        | [] => []
        }

      /* Slot: skip one item, continue */
      | [MatchSlot, ...rest] =>
        switch (items) {
        | [_, ...remaining] => walk_items(remaining, rest)
        | [] => []
        }

      /* Ellipsis: try matching rest at current position (zero skip)
         and also skip forward one at a time */
      | [MatchEllipsis, ...rest] =>
        let here = walk_items(items, rest);
        let skipped =
          switch (items) {
          | [_, ...remaining] => walk_items(remaining, steps)
          | [] => []
          };
        here @ skipped;

      /* Closing delimiter: done, ignore */
      | [MatchDelimiter("]")]
      | [MatchDelimiter(")")] => []

      | _ => []
      };
    let _ = whole;
    walk_items(items, steps);
  }

  /* List spine delegates to generic sequence walker */
  and walk_list_spine =
      (items: list(Exp.t), whole: Exp.t, steps: sem_selector) =>
    walk_seq_spine(items, whole, steps)

  /* Walk a module spine: { item1 ; item2 ; ... }
     Supports positional navigation (Slot, Ellipsis, Focus),
     semicolons as separators, closing brace, and keyword-based
     item matching (let/type/module). */
  and walk_mod_spine =
      (items: list(Mod.t), _whole: Exp.t, steps: sem_selector) => {
    let rec walk_items = (items: list(Mod.t), steps: sem_selector) =>
      switch (steps) {
      /* Focus: return current (first remaining) item */
      | [MatchFocus, ..._] =>
        switch (items) {
        | [item, ..._] => [mk_mod(item)]
        | [] => []
        }

      /* Ellipsis + Focus: skip to last, focus on it */
      | [MatchEllipsis, MatchFocus, ..._] =>
        switch (List.rev(items)) {
        | [last, ..._] => [mk_mod(last)]
        | [] => []
        }

      /* Slot: skip one item, continue */
      | [MatchSlot, ...rest] =>
        switch (items) {
        | [_, ...remaining] => walk_items(remaining, rest)
        | [] => []
        }

      /* Ellipsis: try matching rest at current position (zero skip)
         and also skip forward one at a time */
      | [MatchEllipsis, ...rest] =>
        let here = walk_items(items, rest);
        let skipped =
          switch (items) {
          | [_, ...remaining] => walk_items(remaining, steps)
          | [] => []
          };
        here @ skipped;

      /* Semicolon separator: transparent, just continue */
      | [MatchDelimiter(";"), ...rest] => walk_items(items, rest)

      /* Closing brace: done */
      | [MatchDelimiter("}")] => []

      /* Keyword matching: delegate to item-specific spine walkers */
      | [MatchKeyword("let"), ...rest] =>
        switch (items) {
        | [item, ..._] =>
          switch (item.term) {
          | ModLet(pat, def) =>
            walk_mod_let_spine(pat, def, _whole, item, rest)
          | _ => []
          }
        | [] => []
        }

      | [MatchKeyword("type"), ...rest] =>
        switch (items) {
        | [item, ..._] =>
          switch (item.term) {
          | ModType(tpat, tdef) =>
            walk_mod_type_spine(tpat, tdef, _whole, item, rest)
          | _ => []
          }
        | [] => []
        }

      | [MatchKeyword("module"), ...rest] =>
        switch (items) {
        | [item, ..._] =>
          switch (item.term) {
          | ModuleMod(mpat, def) =>
            walk_mod_module_spine(mpat, def, _whole, item, rest)
          | _ => []
          }
        | [] => []
        }

      | _ => []
      };
    let all = walk_items(items, steps);
    /* Deduplicate by focused_id (ellipsis can reach same item from
       multiple starting positions) */
    let seen = Hashtbl.create(List.length(all));
    List.filter(
      ({focused_id, _}) =>
        if (Hashtbl.mem(seen, focused_id)) {
          false;
        } else {
          Hashtbl.add(seen, focused_id, ());
          true;
        },
      all,
    );
  }

  /* Walk a ModLet spine inside a module body.
     Handles: let <name> = %, let %, let _ = % */
  and walk_mod_let_spine =
      (
        pat: Pat.t,
        def: Exp.t,
        whole: Exp.t,
        _item: Mod.t,
        steps: sem_selector,
      ) => {
    let _ = whole;
    let name_opt = pat_name(pat);
    let name_matches = n => Option.equal(String.equal, name_opt, Some(n));
    switch (steps) {
    /* let <name> = % : named item, focus on def */
    | [MatchName(name), MatchDelimiter("="), MatchFocus]
        when name_matches(name) => [
        mk_exp(~bc="let " ++ name ++ " = ...", def),
      ]
    /* let <name> = <more> : named, continue into def */
    | [MatchName(name), MatchDelimiter("="), ...rest]
        when name_matches(name) =>
      walk(rest, def)
    /* let % : focus on pattern */
    | [MatchFocus] => [mk_pat(~bc="let (pat)", pat)]
    /* let _ = % : wildcard, focus on def */
    | [MatchSlot, MatchDelimiter("="), MatchFocus] => [
        mk_exp(~bc="let _ = ...", def),
      ]
    /* let _ = <more> : wildcard, continue into def */
    | [MatchSlot, MatchDelimiter("="), ...rest] => walk(rest, def)
    | _ => []
    };
  }

  /* Walk a ModType spine inside a module body.
     Handles: type <name> = %, type <name> (tpat focus) */
  and walk_mod_type_spine =
      (
        tpat: TPat.t,
        tdef: Typ.t,
        _whole: Exp.t,
        _item: Mod.t,
        steps: sem_selector,
      ) => {
    let name_opt = tpat_name(tpat);
    let name_matches = n => Option.equal(String.equal, name_opt, Some(n));
    switch (steps) {
    /* type <name> = % : named, focus on type def */
    | [MatchName(name), MatchDelimiter("="), MatchFocus]
        when name_matches(name) => [
        mk_typ(~bc="type " ++ name ++ " = ...", tdef),
      ]
    /* type <name> : focus on tpat */
    | [MatchName(name)] when name_matches(name) => [
        mk_pat(~bc="type " ++ name ++ " (tpat)", tpat_to_pat(tpat)),
      ]
    /* type % : focus on tpat */
    | [MatchFocus] => [mk_pat(~bc="type (tpat)", tpat_to_pat(tpat))]
    | _ => []
    };
  }

  /* Walk a ModuleMod spine inside a module body.
     Handles: module <name> = %, module <name> (focus) */
  and walk_mod_module_spine =
      (
        mpat: TermBase.mpat_t,
        def: Exp.t,
        _whole: Exp.t,
        _item: Mod.t,
        steps: sem_selector,
      ) => {
    let name_opt = mpat_name(mpat);
    let name_matches = n => Option.equal(String.equal, name_opt, Some(n));
    switch (steps) {
    /* module <name> = % : named, focus on def */
    | [MatchName(name), MatchDelimiter("="), MatchFocus]
        when name_matches(name) => [
        mk_exp(~bc="module " ++ name ++ " = ...", def),
      ]
    /* module <name> = <more> : named, continue into def */
    | [MatchName(name), MatchDelimiter("="), ...rest]
        when name_matches(name) =>
      walk(rest, def)
    /* module <name> : focus on item */
    | [MatchName(name)] when name_matches(name) => [
        mk_exp(~bc="module " ++ name, def),
      ]
    | _ => []
    };
  }

  /* === Cross-sort walkers for ChildIndex traversal === */

  /* Walk selector steps against a pattern node */
  and walk_pat = (steps: sem_selector, current: Pat.t): list(match_result) =>
    switch (steps) {
    | []
    | [MatchFocus] => [mk_pat(current)]
    | [MatchFocus, ...rest] => walk_pat(rest, current)
    | [ChildIndex(n), ...rest] =>
      switch (nth_child_pat(n, current)) {
      | Some(FocusPat(p)) => walk_pat(rest, p)
      | Some(FocusTyp(t)) => walk_typ(rest, t)
      | Some(FocusExp(e)) => walk(rest, e)
      | Some(FocusMod(_))
      | None => []
      }
    | _ => []
    }

  /* Walk selector steps against a type node */
  and walk_typ = (steps: sem_selector, current: Typ.t): list(match_result) =>
    switch (steps) {
    | []
    | [MatchFocus] => [mk_typ(current)]
    | [MatchFocus, ...rest] => walk_typ(rest, current)
    | [ChildIndex(n), ...rest] =>
      switch (nth_child_typ(n, current)) {
      | Some(FocusTyp(t)) => walk_typ(rest, t)
      | Some(FocusPat(p)) => walk_pat(rest, p)
      | Some(FocusExp(e)) => walk(rest, e)
      | Some(FocusMod(_))
      | None => []
      }
    | _ => []
    }

  /* Walk selector steps against a module item */
  and walk_mod = (steps: sem_selector, current: Mod.t): list(match_result) =>
    switch (steps) {
    | []
    | [MatchFocus] => [mk_mod(current)]
    | [MatchFocus, ...rest] => walk_mod(rest, current)
    | [ChildIndex(n), ...rest] =>
      switch (nth_child_mod(n, current)) {
      | Some(FocusExp(e)) => walk(rest, e)
      | Some(FocusPat(p)) => walk_pat(rest, p)
      | Some(FocusTyp(t)) => walk_typ(rest, t)
      | Some(FocusMod(m)) => walk_mod(rest, m)
      | None => []
      }
    | _ => []
    }

  /* Find the Let/ModuleExp node for a given binder name */
  and find_let_node = (name: string, e: Exp.t): option(Exp.t) =>
    switch (Exp.term_of(e)) {
    | Let(pat, _def, body) =>
      switch (pat_name(pat)) {
      | Some(n) when String.equal(n, name) => Some(e)
      | _ => find_let_node(name, body)
      }
    | ModuleExp(mpat, _def, body) =>
      switch (mpat_name(mpat)) {
      | Some(n) when String.equal(n, name) => Some(e)
      | _ => find_let_node(name, body)
      }
    | TyAlias(tpat, _tdef, body) =>
      switch (tpat_name(tpat)) {
      | Some(n) when String.equal(n, name) => Some(e)
      | _ => find_let_node(name, body)
      }
    | Parens(inner) => find_let_node(name, inner)
    | _ => None
    }

  /* Find all Let/ModuleExp nodes in an expression chain */
  and find_all_lets = (e: Exp.t): list(let_spine) =>
    switch (Exp.term_of(e)) {
    | Let(pat, def, body) => [
        {
          pat,
          def,
          body,
          whole: e,
        },
        ...find_all_lets(body),
      ]
    | ModuleExp(_, _, body) =>
      /* Skip module bindings (use `module` keyword or chain sugar M/x).
         Continue searching through the body for lets after the module. */
      find_all_lets(body)
    | TyAlias(_, _, body) => find_all_lets(body)
    | Module(items) =>
      /* Surface ModLet items as let_spines so `let x = %` finds them.
         Synthesize EmptyHole body since ModLet has no body. */
      List.concat_map(
        (item: Mod.t) =>
          switch (item.term) {
          | ModLet(pat, def) => [
              {
                pat,
                def,
                body: Exp.fresh(EmptyHole),
                whole: Exp.fresh(Let(pat, def, Exp.fresh(EmptyHole))),
              },
            ]
          | _ => []
          },
        items,
      )
    | _ => []
    }

  /* Find all module-like binders in an expression chain.
     Returns tuples matching walk_after_module_kw's parameters. */
  and find_all_modules =
      (e: Exp.t)
      : list((option(string), Exp.t, Exp.t, option(Exp.t), option(Mod.t))) =>
    switch (Exp.term_of(e)) {
    | ModuleExp(mpat, def, body) => [
        (mpat_name(mpat), def, e, Some(body), None),
        ...find_all_modules(body),
      ]
    | Let(pat, def, body) =>
      switch (Exp.term_of(def)) {
      | Module(_) => [
          (pat_name(pat), def, e, Some(body), None),
          ...find_all_modules(body),
        ]
      | _ => find_all_modules(body)
      }
    | TyAlias(_, _, body) => find_all_modules(body)
    | Module(items) =>
      List.concat_map(
        (item: Mod.t) =>
          switch (item.term) {
          | ModuleMod(mpat, def) => [
              (mpat_name(mpat), def, e, None, Some(item)),
            ]
          | _ => []
          },
        items,
      )
    | _ => []
    }

  /* Find all type-alias-like binders in an expression chain.
     Returns tuples matching walk_after_type_kw's parameters. */
  and find_all_types =
      (e: Exp.t)
      : list((TPat.t, Exp.t, option(Exp.t), option(Typ.t), option(Mod.t))) =>
    switch (Exp.term_of(e)) {
    | TyAlias(tpat, tdef, body) => [
        (tpat, e, Some(body), Some(tdef), None),
        ...find_all_types(body),
      ]
    | Let(_, _, body)
    | ModuleExp(_, _, body) => find_all_types(body)
    | Module(items) =>
      List.concat_map(
        (item: Mod.t) =>
          switch (item.term) {
          | ModType(tpat, tdef) => [
              (tpat, e, None, Some(tdef), Some(item)),
            ]
          | _ => []
          },
        items,
      )
    | _ => []
    }

  /* Descendant search: try matching remaining steps at every
     subexpression in the tree */
  and descend_all = (e: Exp.t, remaining: sem_selector): list(match_result) => {
    /* Try matching at this node */
    let here = walk(remaining, e);
    /* Recurse into children */
    let children =
      switch (Exp.term_of(e)) {
      | Let(_, def, body) =>
        descend_all(def, remaining) @ descend_all(body, remaining)
      | ModuleExp(_, def, body) =>
        descend_all(def, remaining) @ descend_all(body, remaining)
      | TyAlias(_, _, body) => descend_all(body, remaining)
      | If(cond, then_, else_) =>
        descend_all(cond, remaining)
        @ descend_all(then_, remaining)
        @ descend_all(else_, remaining)
      | Match(scrut, rules) =>
        descend_all(scrut, remaining)
        @ List.concat_map(
            ((_, body)) => descend_all(body, remaining),
            rules,
          )
      | Ap(_, fn, arg) =>
        descend_all(fn, remaining) @ descend_all(arg, remaining)
      | Fun(_, body, _, _) => descend_all(body, remaining)
      | Test(body) => descend_all(body, remaining)
      | Tuple(es) => List.concat_map(e => descend_all(e, remaining), es)
      | ListLit(es) => List.concat_map(e => descend_all(e, remaining), es)
      | Seq(e1, e2) =>
        descend_all(e1, remaining) @ descend_all(e2, remaining)
      | Parens(e) => descend_all(e, remaining)
      | BinOp(_, e1, e2) =>
        descend_all(e1, remaining) @ descend_all(e2, remaining)
      | UnOp(_, e) => descend_all(e, remaining)
      | Module(items) =>
        List.concat_map(
          (item: Mod.t) =>
            switch (item.term) {
            | ModLet(_, def) => descend_all(def, remaining)
            | ModExp(e) => descend_all(e, remaining)
            | ModuleMod(_, def) => descend_all(def, remaining)
            | _ => []
            },
          items,
        )
      | Asc(e, _) => descend_all(e, remaining)
      | _ => []
      };
    /* Deduplicate by focused_id: the same node can be reached via both
       walk (which follows body chains) and descend_all (which recurses
       into children). Keep the first occurrence. */
    let all = here @ children;
    let seen = Hashtbl.create(List.length(all));
    List.filter(
      ({focused_id, _}) =>
        if (Hashtbl.mem(seen, focused_id)) {
          false;
        } else {
          Hashtbl.add(seen, focused_id, ());
          true;
        },
      all,
    );
  };

  walk(steps, root);
};

/* Resolve a surface selector against an expression */
let resolve = (sel: selector, root: Exp.t): list(match_result) =>
  resolve_sem(elaborate(sel), root);

/* === Convenience: parse + resolve === */

let query = (selector_str: string, root: Exp.t): list(match_result) => {
  let sel = parse(selector_str);
  resolve(sel, root);
};

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

/* Diagnose why a selector produced no matches.
   Returns a helpful error message with:
   - How far the selector matched before failing
   - Available names at the failure point
   - "Did you mean?" suggestions for close name mismatches */
let diagnose_no_match = (selector_str: string, root: Exp.t): string => {
  let sel = parse(selector_str);
  let steps = elaborate(sel);
  let n = List.length(steps);

  /* Try progressively longer prefixes to find where matching stops.
     For each prefix, append a MatchFocus so resolve returns results
     if the prefix successfully navigates to some subtree. */
  let rec find_last_match = (len: int): (int, list(match_result)) =>
    if (len <= 0) {
      (0, []);
    } else {
      let prefix = List.filteri((i, _) => i < len, steps);
      /* Skip if prefix ends with DescendInto (needs more context) */
      switch (List.rev(prefix)) {
      | [DescendInto, ..._] => find_last_match(len - 1)
      | _ =>
        let probe = prefix @ [MatchFocus];
        let matches = resolve_sem(probe, root);
        if (List.length(matches) > 0) {
          (len, matches);
        } else {
          find_last_match(len - 1);
        };
      };
    };

  let (matched_depth, _context_matches) = find_last_match(n - 1);

  /* Build the error message */
  let base_msg = "No match for selector: " ++ selector_str;

  /* If we matched some prefix, report the failing step */
  let partial_msg =
    if (matched_depth > 0 && matched_depth < n) {
      let matched_part =
        List.filteri((i, _) => i < matched_depth, steps) |> steps_to_string;
      let failing_step =
        switch (List.nth_opt(steps, matched_depth)) {
        | Some(s) => step_to_string(s)
        | None => "?"
        };
      "\n  Matched up to: "
      ++ matched_part
      ++ "\n  Failed at: "
      ++ failing_step;
    } else if (matched_depth == 0 && n > 0) {
      let failing_step =
        switch (List.nth_opt(steps, 0)) {
        | Some(s) => step_to_string(s)
        | None => "?"
        };
      "\n  Failed at first step: " ++ failing_step;
    } else {
      "";
    };

  /* If the failing step is a name, suggest alternatives */
  let name_suggestion =
    switch (List.nth_opt(steps, matched_depth)) {
    | Some(MatchName(target_name)) =>
      /* Find the context expression at the matched depth.
         Use the context_matches from the prefix to get available names. */
      let context_expr =
        if (matched_depth > 0) {
          let prefix = List.filteri((i, _) => i < matched_depth, steps);
          let probe = prefix @ [MatchFocus];
          let matches = resolve_sem(probe, root);
          switch (matches) {
          | [{focused: FocusExp(e), _}, ..._] => e
          | _ => root
          };
        } else {
          root;
        };
      let available = collect_binder_names(context_expr);
      let suggestion =
        switch (suggest_similar_names(target_name, available)) {
        | Some(suggested) => "\n  Did you mean: " ++ suggested
        | None => ""
        };
      let available_str =
        switch (available) {
        | [] => ""
        | names => "\n  Available names: " ++ String.concat(", ", names)
        };
      suggestion ++ available_str;
    | Some(MatchNameIndex(target_name, idx)) =>
      let context_expr =
        if (matched_depth > 0) {
          let prefix = List.filteri((i, _) => i < matched_depth, steps);
          let probe = prefix @ [MatchFocus];
          let matches = resolve_sem(probe, root);
          switch (matches) {
          | [{focused: FocusExp(e), _}, ..._] => e
          | _ => root
          };
        } else {
          root;
        };
      let all_matches = find_all_binders_named(target_name, context_expr);
      let count = List.length(all_matches);
      if (count == 0) {
        "\n  No bindings named '" ++ target_name ++ "'";
      } else {
        "\n  Only "
        ++ string_of_int(count)
        ++ " binding(s) named '"
        ++ target_name
        ++ "' (requested index "
        ++ string_of_int(idx)
        ++ ")";
      };
    | Some(EnterBinderDef(target_name)) =>
      let available = collect_binder_names(root);
      let suggestion =
        switch (suggest_similar_names(target_name, available)) {
        | Some(suggested) => "\n  Did you mean: " ++ suggested
        | None => ""
        };
      let available_str =
        switch (available) {
        | [] => ""
        | names => "\n  Available names: " ++ String.concat(", ", names)
        };
      suggestion ++ available_str;
    | _ => ""
    };

  base_msg ++ partial_msg ++ name_suggestion;
};

/* For edit actions: require exactly one match */
let query_unique =
    (selector_str: string, root: Exp.t): result(match_result, string) => {
  switch (query(selector_str, root)) {
  | [] => Error(diagnose_no_match(selector_str, root))
  | [m] => Ok(m)
  | matches =>
    let n = List.length(matches);
    let crumbs =
      matches
      |> List.map((m: match_result) => m.breadcrumb)
      |> List.filter(s => String.length(s) > 0)
      |> String.concat(", ");
    Error(
      "Ambiguous selector: "
      ++ selector_str
      ++ " matched "
      ++ string_of_int(n)
      ++ " locations"
      ++ (String.length(crumbs) > 0 ? " [" ++ crumbs ++ "]" : ""),
    );
  };
};

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

/* === Canonical numeric path generation ===
   Given a target node ID and a root expression, produce the unique
   ChildIndex path (list of child indices) to that node via DFS.
   Mirrors the child enumeration in nth_child_exp/pat/typ/mod. */

let rec find_in_exp = (target: Id.t, e: Exp.t): option(list(int)) =>
  if (Exp.rep_id(e) == target) {
    Some([]);
  } else {
    /* Build list of (index, focus_target) for all children */
    let children =
      switch (Exp.term_of(e)) {
      /* 0 children */
      | Invalid(_)
      | EmptyHole
      | Deferral(_)
      | Undefined
      | Atom(_)
      | Constructor(_, _)
      | Var(_)
      | BuiltinFun(_)
      | Label(_)
      | ExplicitNonlabel
      | LivelitName(_)
      | MultiHole(_) => []

      /* 1 Exp child */
      | DynamicErrorHole(e1, _)
      | UnOp(_, e1)
      | Test(e1)
      | Parens(e1)
      | Projector(_, e1)
      | Closure(_, e1)
      | ProofObject(e1)
      | Filter(_, e1)
      | TypFun(_, e1, _) => [(0, FocusExp(e1))]

      /* 2 Exp children */
      | BinOp(_, e1, e2)
      | Seq(e1, e2)
      | Cons(e1, e2)
      | ListConcat(e1, e2)
      | Dot(e1, e2)
      | TupLabel(e1, e2)
      | TupleExtension(e1, e2)
      | HintedTest(e1, e2)
      | Ap(_, e1, e2) => [(0, FocusExp(e1)), (1, FocusExp(e2))]

      /* Pat, Exp */
      | Fun(p, body, _, _)
      | FixF(p, body, _)
      | Forall(p, body) => [(0, FocusPat(p)), (1, FocusExp(body))]

      /* Exp, Typ */
      | Asc(e1, t)
      | TypAp(e1, t) => [(0, FocusExp(e1)), (1, FocusTyp(t))]
      /* Typ, Exp */
      | Use(t, body) => [(0, FocusTyp(t)), (1, FocusExp(body))]

      /* Pat, Exp, Exp */
      | Let(p, def, body)
      | Theorem(p, def, body) => [
          (0, FocusPat(p)),
          (1, FocusExp(def)),
          (2, FocusExp(body)),
        ]
      /* 3 Exp children */
      | If(e1, e2, e3) => [
          (0, FocusExp(e1)),
          (1, FocusExp(e2)),
          (2, FocusExp(e3)),
        ]
      /* Typ, Exp (TPat/MPat skipped) */
      | TyAlias(_, t, body) => [(0, FocusTyp(t)), (1, FocusExp(body))]
      | ModuleExp(_, def, body) => [
          (0, FocusExp(def)),
          (1, FocusExp(body)),
        ]

      /* Variable-length */
      | Tuple(items)
      | ListLit(items) => List.mapi((i, e') => (i, FocusExp(e')), items)
      | DeferredAp(fn, args) => [
          (0, FocusExp(fn)),
          ...List.mapi((i, a) => (i + 1, FocusExp(a)), args),
        ]
      /* Match: #0=scrut, then virtual rule pairs at #1, #2, ... */
      | Match(scrut, rules) =>
        let scrut_child = [(0, FocusExp(scrut))];
        let rule_children =
          List.mapi((i, (pat, body)) => (i + 1, (pat, body)), rules);
        /* Search scrut as normal child, rules handled below */
        ignore(rule_children);
        scrut_child;
      /* Module items */
      | Module(items) => List.mapi((i, m) => (i, FocusMod(m)), items)
      };
    /* Search normal children */
    let found =
      List.fold_left(
        (acc, (i, child)) =>
          switch (acc) {
          | Some(_) => acc
          | None => search_child(target, i, child)
          },
        None,
        children,
      );
    /* For Match, also search rule pairs */
    switch (found) {
    | Some(_) => found
    | None =>
      switch (Exp.term_of(e)) {
      | Match(_, rules) => search_rules(target, rules, 0)
      | _ => None
      }
    };
  }

and search_child =
    (target: Id.t, i: int, child: focus_target): option(list(int)) => {
  let result =
    switch (child) {
    | FocusExp(e) => find_in_exp(target, e)
    | FocusPat(p) => find_in_pat(target, p)
    | FocusTyp(t) => find_in_typ(target, t)
    | FocusMod(m) => find_in_mod(target, m)
    };
  Option.map(path => [i, ...path], result);
}

and search_rules =
    (target: Id.t, rules: list((Pat.t, Exp.t)), idx: int)
    : option(list(int)) =>
  switch (rules) {
  | [] => None
  | [(pat, body), ...rest] =>
    /* Rule pair at index idx+1, pat=#0, body=#1 within */
    let rule_idx = idx + 1;
    switch (find_in_pat(target, pat)) {
    | Some(path) => Some([rule_idx, 0, ...path])
    | None =>
      switch (find_in_exp(target, body)) {
      | Some(path) => Some([rule_idx, 1, ...path])
      | None => search_rules(target, rest, idx + 1)
      }
    };
  }

and find_in_pat = (target: Id.t, p: Pat.t): option(list(int)) =>
  if (Pat.rep_id(p) == target) {
    Some([]);
  } else {
    let children =
      switch (Pat.term_of(p)) {
      | Invalid(_)
      | EmptyHole
      | Wild
      | Atom(_)
      | Constructor(_, _)
      | Var(_)
      | Label(_)
      | ExplicitNonlabel
      | MultiHole(_) => []
      | Parens(p1)
      | Projector(_, p1) => [(0, FocusPat(p1))]
      | Cons(p1, p2)
      | TupLabel(p1, p2)
      | Ap(p1, p2) => [(0, FocusPat(p1)), (1, FocusPat(p2))]
      | Asc(p1, t) => [(0, FocusPat(p1)), (1, FocusTyp(t))]
      | Tuple(items)
      | ListLit(items) => List.mapi((i, p') => (i, FocusPat(p')), items)
      };
    List.fold_left(
      (acc, (i, child)) =>
        switch (acc) {
        | Some(_) => acc
        | None => search_child(target, i, child)
        },
      None,
      children,
    );
  }

and find_in_typ = (target: Id.t, t: Typ.t): option(list(int)) =>
  if (Typ.rep_id(t) == target) {
    Some([]);
  } else {
    let children =
      switch (Typ.term_of(t)) {
      | Unknown(_)
      | Atom(_)
      | Var(_)
      | Label(_)
      | ExplicitNonlabel
      | Sum(_)
      | Sig(_) => []
      | List(t1)
      | Parens(t1)
      | Projector(_, t1)
      | Rec(_, t1)
      | Poly(_, t1) => [(0, FocusTyp(t1))]
      | Arrow(t1, t2)
      | TupLabel(t1, t2)
      | ProdProjection(t1, t2)
      | ProdExtension(t1, t2) => [(0, FocusTyp(t1)), (1, FocusTyp(t2))]
      | ProofOf(e) => [(0, FocusExp(e))]
      | Prod(items) => List.mapi((i, t') => (i, FocusTyp(t')), items)
      };
    List.fold_left(
      (acc, (i, child)) =>
        switch (acc) {
        | Some(_) => acc
        | None => search_child(target, i, child)
        },
      None,
      children,
    );
  }

and find_in_mod = (target: Id.t, m: Mod.t): option(list(int)) =>
  if (Mod.rep_id(m) == target) {
    Some([]);
  } else {
    let children =
      switch (m.term) {
      | Invalid(_)
      | EmptyHole
      | MultiHole(_) => []
      | ModLet(p, def) => [(0, FocusPat(p)), (1, FocusExp(def))]
      | ModType(_, t) => [(0, FocusTyp(t))]
      | ModuleMod(_, def) => [(0, FocusExp(def))]
      | ModExp(e) => [(0, FocusExp(e))]
      };
    List.fold_left(
      (acc, (i, child)) =>
        switch (acc) {
        | Some(_) => acc
        | None => search_child(target, i, child)
        },
      None,
      children,
    );
  };

/* Convert a numeric index path to a sem_selector */
let canonical_numeric = (target: Id.t, root: Exp.t): option(sem_selector) =>
  find_in_exp(target, root)
  |> Option.map(indices =>
       List.map(i => ChildIndex(i), indices) @ [MatchFocus]
     );

/* === Named canonical path generation ===
   Prefer human-readable name-based steps where possible, falling back to
   ChildIndex for anonymous subexpressions. Uses binder names for Let/Fun/
   TyAlias/ModuleExp, keyword patterns for If/Match/Fun/Test. */

/* Helper: get the pattern name from a Pat.t, if it's a simple variable */
let pat_name_opt = (p: Pat.t): option(string) =>
  switch (Pat.term_of(p)) {
  | Var(name) => Some(name)
  | Asc(p1, _) =>
    switch (Pat.term_of(p1)) {
    | Var(name) => Some(name)
    | _ => None
    }
  | _ => None
  };

/* Count how many binders with the given name appear before position idx
   in the let-chain starting at root. Returns the shadowed index (0-based). */
let rec count_name_before = (name: string, e: Exp.t, target_id: Id.t): int =>
  switch (Exp.term_of(e)) {
  | Let(pat, def, body) =>
    let is_target_in_def =
      find_in_exp(target_id, def) != None
      || find_in_pat(target_id, pat) != None;
    if (is_target_in_def) {
      0;
      /* Target is in this let's def/pat — count is 0 for binders above */
    } else {
      let increment =
        switch (pat_name_opt(pat)) {
        | Some(n) when n == name => 1
        | _ => 0
        };
      increment + count_name_before(name, body, target_id);
    };
  | _ => 0
  };

/* chain_root: the outermost expression in the current let-chain,
   used for counting shadowed names across the entire chain */
let rec named_in_exp =
        (~chain_root=?, target: Id.t, e: Exp.t): option(sem_selector) => {
  let cr =
    switch (chain_root) {
    | Some(r) => r
    | None => e
    };
  if (Exp.rep_id(e) == target) {
    Some([MatchFocus]);
  } else {
    switch (Exp.term_of(e)) {
    /* Named binders: prefer name-based addressing */
    | Let(pat, def, body) =>
      switch (pat_name_opt(pat)) {
      | Some(name) =>
        /* Target in pat? */
        switch (find_in_pat(target, pat)) {
        | Some(_indices) =>
          /* Focus on the pattern: "let <name>" */
          let name_step = make_name_step(name, cr, target);
          Some([MatchKeyword("let"), name_step, MatchFocus]);
        | None =>
          /* Target in def? Use "<name> = ..." */
          switch (named_in_exp(target, def)) {
          | Some(inner) =>
            let name_step = make_name_step(name, cr, target);
            Some([name_step, MatchDelimiter("="), ...inner]);
          | None =>
            /* Target in body? Continue down the let chain, preserving chain_root */
            named_in_exp(~chain_root=cr, target, body)
          }
        }
      | None =>
        /* No name — fall back to numeric */
        numeric_fallback(target, e)
      }

    /* If: keyword-based addressing */
    | If(cond, then_, else_) =>
      switch (find_in_exp(target, cond)) {
      | Some(idx_path) =>
        Some([MatchKeyword("if")] @ idx_to_steps(idx_path) @ [MatchFocus])
      | None =>
        switch (find_in_exp(target, then_)) {
        | Some(idx_path) =>
          Some(
            [MatchKeyword("if"), MatchSlot, MatchKeyword("then")]
            @ idx_to_steps(idx_path)
            @ [MatchFocus],
          )
        | None =>
          switch (find_in_exp(target, else_)) {
          | Some(idx_path) =>
            Some(
              [MatchKeyword("if"), MatchEllipsis, MatchKeyword("else")]
              @ idx_to_steps(idx_path)
              @ [MatchFocus],
            )
          | None => None
          }
        }
      }

    /* Fun: keyword-based */
    | Fun(pat, body, _, _) =>
      switch (find_in_pat(target, pat)) {
      | Some(_) => Some([MatchKeyword("fun"), MatchFocus])
      | None =>
        switch (named_in_exp(target, body)) {
        | Some(inner) =>
          Some([
            MatchKeyword("fun"),
            MatchSlot,
            MatchDelimiter("->"),
            ...inner,
          ])
        | None => None
        }
      }

    /* Match: keyword-based */
    | Match(scrut, rules) =>
      switch (find_in_exp(target, scrut)) {
      | Some(idx_path) =>
        Some(
          [MatchKeyword("case")] @ idx_to_steps(idx_path) @ [MatchFocus],
        )
      | None => named_in_rules(target, rules)
      }

    /* Test: keyword-based */
    | Test(body) =>
      switch (named_in_exp(target, body)) {
      | Some(inner) => Some([MatchKeyword("test"), ...inner])
      | None => None
      }

    /* Seq: target in first or second */
    | Seq(e1, e2) =>
      switch (named_in_exp(target, e1)) {
      | Some(inner) => Some(inner)
      | None => named_in_exp(target, e2)
      }

    /* Module: search items with named addressing */
    | Module(items) => named_in_module_items(target, items)

    /* TyAlias: name-based */
    | TyAlias(tpat, tdef, body) =>
      let tname =
        switch (tpat.term) {
        | Var(n) => Some(n)
        | _ => None
        };
      switch (tname) {
      | Some(name) =>
        switch (find_in_typ(target, tdef)) {
        | Some(idx_path) =>
          Some(
            [MatchKeyword("type"), MatchName(name), MatchDelimiter("=")]
            @ idx_to_steps(idx_path)
            @ [MatchFocus],
          )
        | None => named_in_exp(~chain_root=cr, target, body)
        }
      | None => numeric_fallback(target, e)
      };

    /* ModuleExp: name-based */
    | ModuleExp(mpat, def, body) =>
      let mname =
        switch (mpat.term) {
        | Var(n) => Some(n)
        | _ => None
        };
      switch (mname) {
      | Some(name) =>
        switch (named_in_exp(target, def)) {
        | Some(inner) =>
          Some([
            MatchKeyword("module"),
            MatchName(name),
            MatchDelimiter("="),
            ...inner,
          ])
        | None => named_in_exp(~chain_root=cr, target, body)
        }
      | None => numeric_fallback(target, e)
      };

    /* Parens: transparent, look inside */
    | Parens(inner) => named_in_exp(~chain_root=cr, target, inner)

    /* BinOp: use operator delimiter for named addressing of operands.
       Only uses operator syntax for immediate operands; deeper targets
       fall back to numeric ChildIndex. */
    | BinOp(op, e1, e2) =>
      let op_str = Operators.bin_op_to_string(op);
      if (is_binop_token(op_str)) {
        if (Exp.rep_id(e1) == target) {
          Some([MatchFocus, MatchDelimiter(op_str), MatchSlot]);
        } else if (Exp.rep_id(e2) == target) {
          Some([MatchSlot, MatchDelimiter(op_str), MatchFocus]);
        } else {
          numeric_fallback(target, e);
        };
      } else {
        numeric_fallback(target, e);
      };

    /* Cons: use :: operator for named addressing of operands */
    | Cons(e1, e2) =>
      if (Exp.rep_id(e1) == target) {
        Some([MatchFocus, MatchDelimiter("::"), MatchSlot]);
      } else if (Exp.rep_id(e2) == target) {
        Some([MatchSlot, MatchDelimiter("::"), MatchFocus]);
      } else {
        numeric_fallback(target, e);
      }

    /* Everything else: fall back to numeric */
    | _ => numeric_fallback(target, e)
    };
  };
}

and make_name_step = (name: string, context: Exp.t, target: Id.t): sem_step => {
  let idx = count_name_before(name, context, target);
  /* Walk to the enclosing let chain root to count all instances of this name */
  let total = count_all_name(name, context);
  if (total > 1) {
    MatchNameIndex(name, idx);
  } else {
    MatchName(name);
  };
}

and count_all_name = (name: string, e: Exp.t): int =>
  switch (Exp.term_of(e)) {
  | Let(pat, _, body) =>
    let here =
      switch (pat_name_opt(pat)) {
      | Some(n) when n == name => 1
      | _ => 0
      };
    here + count_all_name(name, body);
  | _ => 0
  }

and idx_to_steps = (indices: list(int)): list(sem_step) =>
  List.map(i => ChildIndex(i), indices)

and numeric_fallback = (target: Id.t, e: Exp.t): option(sem_selector) =>
  find_in_exp(target, e)
  |> Option.map(indices =>
       List.map(i => ChildIndex(i), indices) @ [MatchFocus]
     )

and named_in_rules =
    (target: Id.t, rules: list((Pat.t, Exp.t))): option(sem_selector) =>
  switch (rules) {
  | [] => None
  | [(pat, body), ...rest] =>
    switch (find_in_pat(target, pat)) {
    | Some(_) =>
      /* Focus on the pattern of this rule */
      /* Use constructor name if available */
      let pat_prefix =
        switch (Pat.term_of(pat)) {
        | Constructor(name, _) => [
            MatchDelimiter("|"),
            MatchEllipsis,
            MatchName(name),
          ]
        | _ => [MatchDelimiter("|"), MatchEllipsis]
        };
      Some(pat_prefix @ [MatchFocus]);
    | None =>
      switch (named_in_exp(target, body)) {
      | Some(inner) =>
        let pat_prefix =
          switch (Pat.term_of(pat)) {
          | Constructor(name, _) => [
              MatchDelimiter("|"),
              MatchEllipsis,
              MatchName(name),
              MatchDelimiter("=>"),
            ]
          | _ => [MatchDelimiter("|"), MatchEllipsis, MatchDelimiter("=>")]
          };
        Some(pat_prefix @ inner);
      | None => named_in_rules(target, rest)
      }
    }
  }

and named_in_module_items =
    (target: Id.t, items: list(Mod.t)): option(sem_selector) =>
  switch (items) {
  | [] => None
  | [item, ...rest] =>
    switch (find_in_mod(target, item)) {
    | Some(_) =>
      /* Found in this item — try name-based addressing */
      switch (item.term) {
      | ModLet(pat, def) =>
        switch (pat_name_opt(pat)) {
        | Some(name) =>
          /* Target is the pat or def */
          switch (find_in_pat(target, pat)) {
          | Some(_) =>
            Some([MatchKeyword("let"), MatchName(name), MatchFocus])
          | None =>
            switch (named_in_exp(target, def)) {
            | Some(inner) =>
              Some([MatchName(name), MatchDelimiter("="), ...inner])
            | None => None
            }
          }
        | None =>
          /* Unnamed — fall back to index */
          let idx = List.length(items) - List.length([item, ...rest]);
          find_in_mod(target, item)
          |> Option.map(indices =>
               [ChildIndex(idx), ...List.map(i => ChildIndex(i), indices)]
               @ [MatchFocus]
             );
        }
      | ModExp(e) =>
        switch (named_in_exp(target, e)) {
        | Some(inner) => Some(inner)
        | None => None
        }
      | _ =>
        /* ModType, ModuleMod — fall back to index for now */
        let idx = List.length(items) - List.length([item, ...rest]);
        find_in_mod(target, item)
        |> Option.map(indices =>
             [ChildIndex(idx), ...List.map(i => ChildIndex(i), indices)]
             @ [MatchFocus]
           );
      }
    | None => named_in_module_items(target, rest)
    }
  };

/* Named canonical: prefer names/keywords over indices */
let canonical_named = (target: Id.t, root: Exp.t): option(sem_selector) =>
  named_in_exp(target, root);

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
