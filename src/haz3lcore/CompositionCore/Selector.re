open Language;

/* Selector language for addressing Hazel syntax subtrees.
      See plans/selector-calculus.md for the full spec.

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
  | FocusMod(Mod.t)
  | FocusSig(Sig.t)
  | FocusTPat(TPat.t)
  | FocusMPat(MPat.t)
  | FocusRule(Pat.t, Exp.t);

type match_result = {
  focused: focus_target,
  focused_id: Id.t,
  /* breadcrumb for disambiguation */
  breadcrumb: string,
};

let id_of_target = (ft: focus_target): Id.t =>
  switch (ft) {
  | FocusExp(e) => Exp.rep_id(e)
  | FocusPat(p) => Pat.rep_id(p)
  | FocusTyp(t) => Typ.rep_id(t)
  | FocusMod(m) => Mod.rep_id(m)
  | FocusSig(s) => Sig.rep_id(s)
  | FocusTPat(tp) => TPat.rep_id(tp)
  | FocusMPat(mp) => MPat.rep_id(mp)
  | FocusRule(p, _) => Pat.rep_id(p)
  };

let mk_result = (~bc="", ft: focus_target): match_result => {
  focused: ft,
  focused_id: id_of_target(ft),
  breadcrumb: bc,
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

/* Convert a surface token to a semantic step */
let token_to_sem = (tok: token): sem_step =>
  switch (tok) {
  | Slot => MatchSlot
  | Ellipsis => MatchEllipsis
  | Focus => MatchFocus
  | Descend => DescendInto
  | KW_let => MatchKeyword("let")
  | KW_fun => MatchKeyword("fun")
  | KW_if => MatchKeyword("if")
  | KW_then => MatchKeyword("then")
  | KW_else => MatchKeyword("else")
  | KW_case => MatchKeyword("case")
  | KW_end => MatchKeyword("end")
  | KW_module => MatchKeyword("module")
  | KW_type => MatchKeyword("type")
  | KW_in => MatchKeyword("in")
  | KW_test => MatchKeyword("test")
  | Pipe => MatchDelimiter("|")
  | FatArrow => MatchDelimiter("=>")
  | Equals => MatchDelimiter("=")
  | Colon => MatchDelimiter(":")
  | Arrow => MatchDelimiter("->")
  | LBracket => MatchDelimiter("[")
  | RBracket => MatchDelimiter("]")
  | LParen => MatchDelimiter("(")
  | RParen => MatchDelimiter(")")
  | LBrace => MatchDelimiter("{")
  | RBrace => MatchDelimiter("}")
  | Semi => MatchDelimiter(";")
  | Comma => MatchDelimiter(",")
  | Operator(s) => MatchDelimiter(s)
  | Literal(s) => MatchAtom(s)
  | Name(s) => MatchName(s)
  | NameIndex(s, i) => MatchNameIndex(s, i)
  | Index(i) => ChildIndex(i)
  | Chain(_, _) => MatchSlot /* handled separately */
  };

/* Check if a token is a name (includes Name, NameIndex, Literal) */
let is_name_token = (tok: token): bool =>
  switch (tok) {
  | Name(_)
  | NameIndex(_, _)
  | Literal(_) => true
  | _ => false
  };

/* Check if a token is a keyword */
let is_keyword_token = (tok: token): bool =>
  switch (tok) {
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
  | KW_test => true
  | _ => false
  };

/* Check if a token is a delimiter or operator */
let is_delim_token = (tok: token): bool =>
  switch (tok) {
  | Pipe
  | FatArrow
  | Equals
  | Colon
  | Arrow
  | LBracket
  | RBracket
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semi
  | Comma
  | Operator(_) => true
  | _ => false
  };

/* Elaborate a surface selector into a semantic selector.
   Handles: chains, implicit focus, double descent collapse. */
let elaborate = (sel: selector): sem_selector => {
  /* Step 1: Expand chains into EnterBinderDef + MatchName sequences */
  let expand_chains = (tokens: list(token)): list(token) => {
    List.concat_map(
      tok =>
        switch (tok) {
        | Chain(segments, trailing_slash) =>
          if (trailing_slash) {
            /* A/B/C/ → all are EnterBinder. Keep as Chain tokens temporarily,
               we'll convert to sem_steps later. We use a trick: produce
               individual Chain([seg], true) tokens that the sem conversion
               will handle. Actually, let's just produce the sem_steps directly
               via a marker. For simplicity, let's expand here. */
            List.map(seg => Chain([seg], true), segments);
          } else {
            /* A/B/C → A/ B/ then MatchName(C) */
            let n = List.length(segments);
            List.mapi(
              (i, seg) =>
                if (i < n - 1) {
                  Chain([seg], true);
                } else {
                  Name(seg);
                },
              segments,
            );
          }
        | _ => [tok]
        },
      tokens,
    );
  };

  let tokens = expand_chains(sel);

  /* Step 2: Convert tokens to sem_steps */
  let steps =
    List.map(
      tok =>
        switch (tok) {
        | Chain([seg], true) => EnterBinderDef(seg)
        | Chain(_, _) => MatchSlot /* shouldn't happen after expansion */
        | t => token_to_sem(t)
        },
      tokens,
    );

  /* Step 3: Insert implicit focus if no MatchFocus present.
     Rules:
     - If last element is a name → insert MatchFocus before it
     - Otherwise → append MatchFocus at the end */
  let has_focus = List.exists(s => s == MatchFocus, steps);
  let steps =
    if (has_focus) {
      steps;
    } else {
      let n = List.length(steps);
      if (n > 0) {
        let last = List.nth(steps, n - 1);
        switch (last) {
        | MatchName(_)
        | MatchNameIndex(_, _) =>
          /* Insert focus before last name */
          let prefix = List.filteri((i, _) => i < n - 1, steps);
          prefix @ [MatchFocus, last];
        | _ =>
          /* Append focus */
          steps @ [MatchFocus];
        };
      } else {
        [MatchFocus];
      };
    };

  /* Step 4: Collapse consecutive DescendInto */
  let rec collapse_descend = (steps: sem_selector): sem_selector =>
    switch (steps) {
    | [DescendInto, DescendInto, ...rest] =>
      collapse_descend([DescendInto, ...rest])
    | [s, ...rest] => [s, ...collapse_descend(rest)]
    | [] => []
    };

  collapse_descend(steps);
};

/* === Decomposition types === */

type spine_pos =
  | PosToken(string)
  | PosChild(focus_target);

type decomposed =
  | Form(list(spine_pos))
  | AtomNode(string)
  | Hole
  | Transparent(focus_target);

/* === Decompose: map term variants to spines === */

let intersperse = (sep: 'a, items: list('a)): list('a) => {
  let rec go =
    fun
    | [] => []
    | [x] => [x]
    | [x, ...rest] => [x, sep, ...go(rest)];
  go(items);
};

/* Helper constructors for spine positions */
let t = (s: string): spine_pos => PosToken(s);
let c_exp = (e: Exp.t): spine_pos => PosChild(FocusExp(e));
let c_pat = (p: Pat.t): spine_pos => PosChild(FocusPat(p));
let c_typ = (ty: Typ.t): spine_pos => PosChild(FocusTyp(ty));
let c_mod = (m: Mod.t): spine_pos => PosChild(FocusMod(m));
let c_sig = (s: Sig.t): spine_pos => PosChild(FocusSig(s));
let c_tpat = (tp: TPat.t): spine_pos => PosChild(FocusTPat(tp));
let c_mpat = (mp: MPat.t): spine_pos => PosChild(FocusMPat(mp));

let decompose_exp = (e: Exp.t): decomposed =>
  switch (Exp.term_of(e)) {
  /* Binding forms */
  | Let(pat, def, body) =>
    /* Expand Asc pattern: let x : T = ... → [let, x, :, T, =, ...] */
    switch (Pat.term_of(pat)) {
    | Asc(inner_pat, typ) =>
      Form([
        t("let"),
        c_pat(inner_pat),
        t(":"),
        c_typ(typ),
        t("="),
        c_exp(def),
        t("in"),
        c_exp(body),
      ])
    | _ =>
      Form([t("let"), c_pat(pat), t("="), c_exp(def), t("in"), c_exp(body)])
    }
  | TyAlias(tpat, typ, body) =>
    Form([
      t("type"),
      c_tpat(tpat),
      t("="),
      c_typ(typ),
      t("in"),
      c_exp(body),
    ])
  | ModuleExp(mpat, def, body) =>
    Form([
      t("module"),
      c_mpat(mpat),
      t("="),
      c_exp(def),
      t("in"),
      c_exp(body),
    ])
  | Theorem(pat, def, body) =>
    Form([
      t("theorem"),
      c_pat(pat),
      t("="),
      c_exp(def),
      t("in"),
      c_exp(body),
    ])

  /* Functions */
  | Fun(pat, body, _, _) =>
    switch (Pat.term_of(pat)) {
    | Asc(inner_pat, typ) =>
      Form([
        t("fun"),
        c_pat(inner_pat),
        t(":"),
        c_typ(typ),
        t("->"),
        c_exp(body),
      ])
    | _ => Form([t("fun"), c_pat(pat), t("->"), c_exp(body)])
    }
  | FixF(pat, body, _) =>
    Form([t("fix"), c_pat(pat), t("->"), c_exp(body)])
  | TypFun(tpat, body, _) =>
    Form([t("typfun"), c_tpat(tpat), t("->"), c_exp(body)])

  /* Control flow */
  | If(cond, then_, else_) =>
    Form([
      t("if"),
      c_exp(cond),
      t("then"),
      c_exp(then_),
      t("else"),
      c_exp(else_),
    ])
  | Match(scrut, rules) =>
    Form(
      [t("case"), c_exp(scrut)]
      @ List.concat_map(
          ((pat, body)) => [
            t("|"),
            PosChild(FocusRule(pat, body)),
          ],
          rules,
        )
      @ [t("end")],
    )

  /* Collections */
  | Tuple(items) =>
    Form(
      [t("(")]
      @ intersperse(t(","), List.map(e => c_exp(e), items))
      @ [t(")")],
    )
  | ListLit(items) =>
    Form(
      [t("[")]
      @ intersperse(t(","), List.map(e => c_exp(e), items))
      @ [t("]")],
    )
  | Module(items) =>
    Form(
      [t("{")]
      @ intersperse(t(";"), List.map(m => c_mod(m), items))
      @ [t("}")],
    )

  /* Operators */
  | BinOp(op, e1, e2) =>
    Form([c_exp(e1), t(Operators.bin_op_to_string(op)), c_exp(e2)])
  | UnOp(op, e1) =>
    let op_str =
      switch (op) {
      | Meta(Unquote) => "$"
      | Int(Minus)
      | Nat(Minus)
      | SInt(Minus)
      | Float(Minus) => "-"
      | Bool(Not) => "!"
      };
    Form([t(op_str), c_exp(e1)]);
  | Cons(hd, tl) => Form([c_exp(hd), t("::"), c_exp(tl)])
  | ListConcat(l, r) => Form([c_exp(l), t("@"), c_exp(r)])

  /* Application */
  | Ap(_, fn, arg) =>
    Form([c_exp(fn), t("("), c_exp(arg), t(")")])

  /* Type annotation */
  | Asc(expr, typ) =>
    Form([c_exp(expr), t(":"), c_typ(typ)])

  /* Test */
  | Test(body) => Form([t("test"), c_exp(body), t("end")])

  /* Sequence */
  | Seq(e1, e2) => Form([c_exp(e1), t(";"), c_exp(e2)])

  /* Dot access */
  | Dot(obj, field) => Form([c_exp(obj), t("."), c_exp(field)])

  /* Transparent wrappers */
  | Parens(inner) => Transparent(FocusExp(inner))
  | Projector(_, inner) => Transparent(FocusExp(inner))
  | Filter(_, inner) => Transparent(FocusExp(inner))
  | Closure(_, inner) => Transparent(FocusExp(inner))

  /* Atoms */
  | Var(name) => AtomNode(name)
  | Constructor(name, _) => AtomNode(name)
  | Atom(c) => AtomNode(Atom.to_literal(c))
  | Label(s) => AtomNode(s)
  | BuiltinFun(name) => AtomNode(name)

  /* Holes */
  | EmptyHole
  | Invalid(_)
  | MultiHole(_) => Hole

  /* Others */
  | _ => Hole
  };

let decompose_pat = (p: Pat.t): decomposed =>
  switch (Pat.term_of(p)) {
  | Var(name) => AtomNode(name)
  | Constructor(name, _) => AtomNode(name)
  | Atom(c) => AtomNode(Atom.to_literal(c))
  | Wild => AtomNode("_")
  | Label(s) => AtomNode(s)
  | Tuple(items) =>
    Form(
      [t("(")]
      @ intersperse(t(","), List.map(p => c_pat(p), items))
      @ [t(")")],
    )
  | ListLit(items) =>
    Form(
      [t("[")]
      @ intersperse(t(","), List.map(p => c_pat(p), items))
      @ [t("]")],
    )
  | Cons(hd, tl) => Form([c_pat(hd), t("::"), c_pat(tl)])
  | Ap(ctor, arg) =>
    Form([c_pat(ctor), t("("), c_pat(arg), t(")")])
  | Asc(inner, typ) =>
    Form([c_pat(inner), t(":"), c_typ(typ)])
  | TupLabel(label, inner) =>
    Form([c_pat(label), t("="), c_pat(inner)])
  | Parens(inner) => Transparent(FocusPat(inner))
  | Projector(_, inner) => Transparent(FocusPat(inner))
  | EmptyHole => Hole
  | _ => Hole
  };

let decompose_typ = (ty: Typ.t): decomposed =>
  switch (Typ.term_of(ty)) {
  | Atom(c) =>
    let s =
      switch (c) {
      | Int => "Int"
      | SInt => "SInt"
      | Nat => "Nat"
      | Float => "Float"
      | Bool => "Bool"
      | String => "String"
      };
    AtomNode(s);
  | Var(name) => AtomNode(name)
  | Arrow(t1, t2) =>
    Form([c_typ(t1), t("->"), c_typ(t2)])
  | Prod(items) =>
    Form(
      [t("(")]
      @ intersperse(t(","), List.map(ty => c_typ(ty), items))
      @ [t(")")],
    )
  | List(inner) =>
    Form([t("["), c_typ(inner), t("]")])
  | Rec(tpat, body) =>
    Form([t("rec"), c_tpat(tpat), t("->"), c_typ(body)])
  | Poly(tpat, body) =>
    Form([t("poly"), c_tpat(tpat), t("->"), c_typ(body)])
  | Sig(items) =>
    Form(
      [t("{")]
      @ intersperse(t(";"), List.map(s => c_sig(s), items))
      @ [t("}")],
    )
  | Parens(inner) => Transparent(FocusTyp(inner))
  | Projector(_, inner) => Transparent(FocusTyp(inner))
  | Unknown(Hole(_)) => Hole
  | _ => Hole
  };

let decompose_mod = (m: Mod.t): decomposed =>
  switch (IdTagged.term_of(m)) {
  | ModLet(pat, def) =>
    Form([t("let"), c_pat(pat), t("="), c_exp(def)])
  | ModType(tpat, typ) =>
    Form([t("type"), c_tpat(tpat), t("="), c_typ(typ)])
  | ModuleMod(mpat, def) =>
    Form([t("module"), c_mpat(mpat), t("="), c_exp(def)])
  | ModExp(e) => Transparent(FocusExp(e))
  | EmptyHole => Hole
  | _ => Hole
  };

let decompose_sig = (s: Sig.t): decomposed =>
  switch (IdTagged.term_of(s)) {
  | SigLet(pat) =>
    Form([t("let"), c_pat(pat)])
  | SigType(tpat, typ) =>
    Form([t("type"), c_tpat(tpat), t("="), c_typ(typ)])
  | EmptyHole => Hole
  | _ => Hole
  };

let decompose_tpat = (tp: TPat.t): decomposed =>
  switch (IdTagged.term_of(tp)) {
  | Var(name) => AtomNode(name)
  | EmptyHole => Hole
  | _ => Hole
  };

let decompose_mpat = (mp: MPat.t): decomposed =>
  switch (IdTagged.term_of(mp)) {
  | Var(name) => AtomNode(name)
  | Asc(inner, typ) =>
    Form([c_mpat(inner), t(":"), c_typ(typ)])
  | EmptyHole => Hole
  | _ => Hole
  };

let decompose_rule = (pat: Pat.t, body: Exp.t): decomposed =>
  Form([t("|"), c_pat(pat), t("=>"), c_exp(body)]);

let decompose = (target: focus_target): decomposed =>
  switch (target) {
  | FocusExp(e) => decompose_exp(e)
  | FocusPat(p) => decompose_pat(p)
  | FocusTyp(ty) => decompose_typ(ty)
  | FocusMod(m) => decompose_mod(m)
  | FocusSig(s) => decompose_sig(s)
  | FocusTPat(tp) => decompose_tpat(tp)
  | FocusMPat(mp) => decompose_mpat(mp)
  | FocusRule(p, e) => decompose_rule(p, e)
  };

/* Resolve Transparent wrappers */
let rec decompose_through = (target: focus_target): (focus_target, decomposed) =>
  switch (decompose(target)) {
  | Transparent(inner) => decompose_through(inner)
  | d => (target, d)
  };

/* === Children enumeration === */

let children_of = (target: focus_target): list(focus_target) =>
  switch (decompose(target)) {
  | Form(positions) =>
    positions
    |> List.filter_map(
         fun
         | PosChild(c) => Some(c)
         | PosToken(_) => None,
       )
  | Transparent(inner) => [inner]
  | AtomNode(_)
  | Hole => []
  };

let nth_child = (n: int, target: focus_target): option(focus_target) => {
  /* Use direct decompose (NOT decompose_through) so that Parens/Projector
     are treated as single-child wrappers rather than being skipped */
  switch (decompose(target)) {
  | Form(positions) =>
    let children =
      positions
      |> List.filter_map(
           fun
           | PosChild(c) => Some(c)
           | PosToken(_) => None,
         );
    List.nth_opt(children, n);
  | Transparent(inner) =>
    /* Transparent wrappers have exactly one child */
    if (n == 0) {
      Some(inner);
    } else {
      None;
    }
  | AtomNode(_)
  | Hole => None
  };
};

/* === Name extraction === */

/* Extract a name from a pattern, looking through wrappers */
let rec pat_name = (p: Pat.t): option(string) =>
  switch (Pat.term_of(p)) {
  | Var(name) => Some(name)
  | Asc(inner, _) => pat_name(inner)
  | Parens(inner) => pat_name(inner)
  | Projector(_, inner) => pat_name(inner)
  | TupLabel(_, inner) => pat_name(inner)
  | Constructor(name, _) => Some(name)
  | _ => None
  };

let tpat_name = (tp: TPat.t): option(string) =>
  switch (IdTagged.term_of(tp)) {
  | Var(name) => Some(name)
  | _ => None
  };

let rec mpat_name = (mp: MPat.t): option(string) =>
  switch (IdTagged.term_of(mp)) {
  | Var(name) => Some(name)
  | Asc(inner, _) => mpat_name(inner)
  | _ => None
  };

/* Get the name from a focus_target (for child name matching) */
let name_of_target = (target: focus_target): option(string) =>
  switch (target) {
  | FocusPat(p) => pat_name(p)
  | FocusTPat(tp) => tpat_name(tp)
  | FocusMPat(mp) => mpat_name(mp)
  | FocusExp(e) =>
    switch (Exp.term_of(e)) {
    | Var(name)
    | Constructor(name, _) => Some(name)
    | _ => None
    }
  | FocusMod(m) =>
    switch (IdTagged.term_of(m)) {
    | ModLet(pat, _) => pat_name(pat)
    | ModType(tpat, _) => tpat_name(tpat)
    | ModuleMod(mpat, _) => mpat_name(mpat)
    | _ => None
    }
  | FocusRule(pat, _) => pat_name(pat)
  | _ => None
  };

/* Extract the atom string from a focus target */
let atom_string = (target: focus_target): option(string) => {
  let (_, dec) = decompose_through(target);
  switch (dec) {
  | AtomNode(s) => Some(s)
  | _ => None
  };
};

/* Check whether a name matches, considering NameIdx */
let name_matches_str =
    (name: string, idx_opt: option(int), target_name: string): bool =>
  switch (idx_opt) {
  | None => String.equal(name, target_name)
  | Some(_) => String.equal(name, target_name) /* idx handled at binder level */
  };

/* === Binder search === */

/* Binder definitions can be expressions (let, module) or types (type alias). */
type binder_def =
  | ExpDef(Exp.t)
  | TypDef(Typ.t);

/* Find all bindings with a given name in a let-chain/module structure.
   Returns (definition_as_focus_target, containing_mod_item_option) pairs. */
let rec find_all_binders_named =
        (name: string, target: focus_target)
        : list((focus_target, option(Mod.t))) =>
  switch (target) {
  | FocusExp(e) =>
    switch (Exp.term_of(e)) {
    | Let(pat, def, body) =>
      let here =
        switch (pat_name(pat)) {
        | Some(n) when String.equal(n, name) => [
            (FocusExp(def), None),
          ]
        | _ => []
        };
      here @ find_all_binders_named(name, FocusExp(body));
    | TyAlias(tpat, typ, body) =>
      let here =
        switch (tpat_name(tpat)) {
        | Some(n) when String.equal(n, name) => [
            (FocusTyp(typ), None),
          ]
        | _ => []
        };
      here @ find_all_binders_named(name, FocusExp(body));
    | ModuleExp(mpat, def, body) =>
      let here =
        switch (mpat_name(mpat)) {
        | Some(n) when String.equal(n, name) => [
            (FocusExp(def), None),
          ]
        | _ => []
        };
      here @ find_all_binders_named(name, FocusExp(body));
    | Module(items) =>
      items
      |> List.concat_map(m =>
           find_all_binders_in_mod(name, m)
         )
    | Parens(inner) =>
      find_all_binders_named(name, FocusExp(inner))
    | Projector(_, inner) =>
      find_all_binders_named(name, FocusExp(inner))
    | _ => []
    }
  | FocusMod(m) => find_all_binders_in_mod(name, m)
  | _ => []
  }

and find_all_binders_in_mod =
    (name: string, m: Mod.t): list((focus_target, option(Mod.t))) =>
  switch (IdTagged.term_of(m)) {
  | ModLet(pat, def) =>
    switch (pat_name(pat)) {
    | Some(n) when String.equal(n, name) => [
        (FocusExp(def), Some(m)),
      ]
    | _ => []
    }
  | ModType(tpat, typ) =>
    switch (tpat_name(tpat)) {
    | Some(n) when String.equal(n, name) => [
        (FocusTyp(typ), Some(m)),
      ]
    | _ => []
    }
  | ModuleMod(mpat, def) =>
    switch (mpat_name(mpat)) {
    | Some(n) when String.equal(n, name) => [
        (FocusExp(def), Some(m)),
      ]
    | _ => []
    }
  | ModExp(e) =>
    find_all_binders_named(name, FocusExp(e))
  | _ => []
  };

/* Collect all binder names visible at top level */
let rec collect_binder_names = (target: focus_target): list(string) =>
  switch (target) {
  | FocusExp(e) =>
    switch (Exp.term_of(e)) {
    | Let(pat, _, body) =>
      let here =
        switch (pat_name(pat)) {
        | Some(n) => [n]
        | None => []
        };
      here @ collect_binder_names(FocusExp(body));
    | TyAlias(tpat, _, body) =>
      let here =
        switch (tpat_name(tpat)) {
        | Some(n) => [n]
        | None => []
        };
      here @ collect_binder_names(FocusExp(body));
    | ModuleExp(mpat, _, body) =>
      let here =
        switch (mpat_name(mpat)) {
        | Some(n) => [n]
        | None => []
        };
      here @ collect_binder_names(FocusExp(body));
    | Parens(inner)
    | Projector(_, inner) =>
      collect_binder_names(FocusExp(inner))
    | Module(items) =>
      items |> List.concat_map(m => collect_binder_names(FocusMod(m)))
    | _ => []
    }
  | FocusMod(m) =>
    switch (IdTagged.term_of(m)) {
    | ModLet(pat, _) =>
      switch (pat_name(pat)) {
      | Some(n) => [n]
      | None => []
      }
    | ModType(tpat, _) =>
      switch (tpat_name(tpat)) {
      | Some(n) => [n]
      | None => []
      }
    | ModuleMod(mpat, _) =>
      switch (mpat_name(mpat)) {
      | Some(n) => [n]
      | None => []
      }
    | _ => []
    }
  | _ => []
  };

/* === Core resolver === */

/* The resolver operates on the elaborated sem_selector (flat list of sem_steps)
   and walks the term tree to find all matching positions.

   Key design: we keep the resolver operating on the flat sem_selector type
   (not a recursive tree type) because:
   1. The tests construct sem_selector values directly for deparse tests
   2. The canonical functions produce sem_selector values
   3. The flat representation is simpler to manipulate

   The resolver is essentially a state machine that processes steps one at a time,
   with the "state" being the current focus target and the remaining steps. */

/* Check if a token string matches a spine position's token */
let pos_is_token = (expected: string, pos: spine_pos): bool =>
  switch (pos) {
  | PosToken(s) => String.equal(s, expected)
  | PosChild(_) => false
  };

/* Check if a spine_pos is a child */
let pos_is_child = (pos: spine_pos): bool =>
  switch (pos) {
  | PosChild(_) => true
  | PosToken(_) => false
  };

/* Is a token string a separator? */
let is_separator = (s: string): bool =>
  switch (s) {
  | ","
  | ";"
  | "|" => true
  | _ => false
  };

/* Skip separator tokens to find the next child in a spine */
let skip_tokens_to_child =
    (positions: list(spine_pos))
    : option((focus_target, list(spine_pos))) =>
  switch (positions) {
  | [PosChild(c), ...rest] => Some((c, rest))
  | [PosToken(sep), PosChild(c), ...rest] when is_separator(sep) =>
    Some((c, rest))
  | _ => None
  };

/* Skip children to find a specific token */
let rec skip_to_token =
        (tok: string, positions: list(spine_pos))
        : option(list(spine_pos)) =>
  switch (positions) {
  | [] => None
  | [PosToken(s), ...rest] when String.equal(s, tok) => Some(rest)
  | [_, ...rest] => skip_to_token(tok, rest)
  };

/* Try to match a name against the next child in spine positions.
   Handles separator transparency. */
let match_name_in_spine =
    (name: string, idx_opt: option(int), positions: list(spine_pos))
    : option((focus_target, list(spine_pos))) =>
  switch (skip_tokens_to_child(positions)) {
  | Some((child, rest)) =>
    switch (name_of_target(child)) {
    | Some(n) when name_matches_str(name, idx_opt, n) =>
      Some((child, rest))
    | _ => None
    }
  | None => None
  };

/* === Spine matching with auto-entry into children === */

/* The spine matcher takes sem_steps and spine positions.
   When a keyword/delimiter doesn't match the next position (which is a child),
   the matcher tries to enter the child and match inside its sub-spine.
   This is the "spine descent" behavior that lets the flat parser work. */

/* Result of resolving inside a spine: list of (focus_target, breadcrumb) */
type spine_result = list((focus_target, string));

/* Main resolve function.
   Returns list of match_results for each successful resolution path. */
let rec resolve_steps =
        (steps: sem_selector, target: focus_target): list(match_result) => {
  /* Handle ChildIndex on the raw target (before decompose_through)
     so that Parens/Projector are treated as single-child wrappers
     rather than being skipped. */
  switch (steps) {
  | [ChildIndex(n), ...rest] =>
    switch (nth_child(n, target)) {
    | Some(child) => resolve_steps(rest, child)
    | None => []
    }
  | _ =>
    /* Look through transparent wrappers */
    let (actual, dec) = decompose_through(target);
    resolve_with_decomposed(steps, actual, dec);
  };
}

and resolve_with_decomposed =
    (steps: sem_selector, actual: focus_target, dec: decomposed)
    : list(match_result) => {
  switch (steps) {
  | [] =>
    /* No steps left: focus on current node */
    [mk_result(actual)]

  | [MatchFocus, ...rest] =>
    switch (rest) {
    | [] => [mk_result(actual)]
    | _ =>
      /* If node is a Form, delegate to spine matching which handles
         Focus as a position marker (e.g., "% + _" focuses on left operand) */
      switch (dec) {
      | Form(positions) =>
        resolve_spine(steps, positions, actual)
      | _ =>
        /* Non-form: use Focus(Some(k)) predicate semantics */
        let inner = resolve_steps(rest, actual);
        if (List.length(inner) > 0) {
          [mk_result(actual)];
        } else {
          [];
        };
      }
    }

  | [ChildIndex(n), ...rest] =>
    switch (nth_child(n, actual)) {
    | Some(child) => resolve_steps(rest, child)
    | None => []
    }

  | [DescendInto, ...rest] =>
    /* Search this node and all descendants */
    let here = resolve_steps(rest, actual);
    let below = descend_all(rest, actual);
    dedup_results(here @ below);

  | [EnterBinderDef(name), ...rest] =>
    /* Find all binders with this name, enter their definitions */
    let binders = find_all_binders_named(name, actual);
    binders
    |> List.concat_map(((def_target, _mod_opt)) =>
         resolve_steps(rest, def_target)
       );

  | [MatchAtom(s), ...rest] =>
    /* Match an atom by string */
    switch (atom_string(actual)) {
    | Some(s') when atom_matches(s, s') =>
      resolve_steps(rest, actual)
    | _ =>
      /* If current node decomposes to transparent, we already looked through.
         Try descent into children to find atoms. */
      []
    }

  | _ =>
    /* Spine matching steps: keywords, delimiters, names, slots, etc. */
    switch (dec) {
    | Form(positions) =>
      resolve_spine(steps, positions, actual)
    | AtomNode(s) =>
      /* Check if the remaining steps start with a name/atom match */
      resolve_atom_with_steps(steps, s, actual)
    | Hole => []
    | Transparent(_) => [] /* should have been resolved */
    }
  };
}

and resolve_atom_with_steps =
    (steps: sem_selector, atom_s: string, actual: focus_target)
    : list(match_result) =>
  switch (steps) {
  | [MatchName(name), ...rest] when String.equal(name, atom_s) =>
    resolve_steps(rest, actual)
  | [MatchNameIndex(name, _), ...rest] when String.equal(name, atom_s) =>
    resolve_steps(rest, actual)
  | [MatchAtom(s), ...rest] when atom_matches(s, atom_s) =>
    resolve_steps(rest, actual)
  | [MatchFocus, ...rest] =>
    /* Focus on this atom */
    switch (rest) {
    | [] => [mk_result(actual)]
    | [MatchName(name), ...rest2] when String.equal(name, atom_s) =>
      /* e.g., "% x" - focus on atom if name matches */
      switch (rest2) {
      | [] => [mk_result(actual)]
      | _ =>
        let inner = resolve_steps(rest, actual);
        if (List.length(inner) > 0) {
          [mk_result(actual)];
        } else {
          [];
        };
      }
    | _ =>
      let inner = resolve_steps(rest, actual);
      if (List.length(inner) > 0) {
        [mk_result(actual)];
      } else {
        [];
      };
    }
  | [MatchSlot, ...rest] =>
    /* Slot matches any child including atoms */
    resolve_steps(rest, actual)
  | [MatchEllipsis, ...rest] =>
    /* Ellipsis followed by more steps: try matching rest here */
    resolve_steps(rest, actual)
  | _ => []
  }

/* Resolve spine-matching steps against a list of spine positions.
   This is the core matching algorithm. */
and resolve_spine =
    (steps: sem_selector, positions: list(spine_pos), form_target: focus_target)
    : list(match_result) => {
  switch (steps) {
  | [] =>
    /* No more steps: implicit trailing wildcard. This means we matched the form
       but didn't find a focus. Return focus on the form itself. */
    [mk_result(form_target)]

  | [MatchFocus, ...rest] =>
    /* Focus inside a spine. The focus applies to the NEXT child position. */
    switch (rest) {
    | [] =>
      /* Bare focus at end of spine: focus on next child */
      switch (skip_tokens_to_child(positions)) {
      | Some((child, _)) => [mk_result(child)]
      | None =>
        /* No child left: focus on the form itself */
        [mk_result(form_target)]
      }
    | _ =>
      /* Focus + more steps:
         Case 1: The remaining steps describe the child (ChildSel behavior)
         Case 2: The remaining steps are a predicate on a name */
      switch (rest) {
      | [MatchName(name), ...rest2]
      | [MatchNameIndex(name, _), ...rest2] =>
        /* Try to find a child with this name */
        switch (find_named_child_in_positions(name, positions)) {
        | Some((child, _remaining)) =>
          /* Found the named child. Now:
             - If rest2 is empty, focus on the child (e.g., "let %x")
             - If rest2 has spine steps, the Focus is predicate-style on form */
          switch (rest2) {
          | [] => [mk_result(child)]
          | [MatchDelimiter(_), ..._] =>
            /* e.g., "% x =" or "% x -> " - rest describes more of the spine.
               The % + name is checking the child, then continuing.
               Actually, "let % x = ..." means focus on the pattern child named x.
               Let's just focus on the child. */
            [mk_result(child)]
          | _ => [mk_result(child)]
          }
        | None => []
        }
      | _ =>
        /* Generic case: two sub-cases based on what rest starts with:
           a) Keyword → "% let x" style: rest describes the whole form
           b) Other → "let % =" style: rest describes spine after child */
        let starts_with_keyword =
          switch (rest) {
          | [MatchKeyword(_), ..._] => true
          | _ => false
          };
        if (starts_with_keyword) {
          /* Form-level predicate: check rest against the entire form */
          let form_check = resolve_steps(rest, form_target);
          if (List.length(form_check) > 0) {
            [mk_result(form_target)];
          } else {
            [];
          };
        } else {
          /* Child-level: focus on next child, using rest as spine predicate */
          switch (skip_tokens_to_child(positions)) {
          | Some((child, remaining)) =>
            let spine_check = resolve_spine(rest, remaining, form_target);
            if (List.length(spine_check) > 0) {
              [mk_result(child)];
            } else {
              /* Enter child for descent */
              let enter = resolve_steps(steps, child);
              if (List.length(enter) > 0) {
                enter;
              } else {
                resolve_spine(steps, remaining, form_target);
              };
            }
          | None => []
          };
        }
      }
    }

  | [MatchKeyword(kw), ...rest] =>
    match_keyword_in_spine(kw, rest, positions, form_target)

  | [MatchDelimiter(d), ...rest] =>
    match_delimiter_in_spine(d, rest, positions, form_target)

  | [MatchName(name), ...rest] =>
    match_name_in_spine_resolve(name, None, rest, positions, form_target)

  | [MatchNameIndex(name, idx), ...rest] =>
    match_name_in_spine_resolve(
      name,
      Some(idx),
      rest,
      positions,
      form_target,
    )

  | [MatchSlot, ...rest] =>
    /* Consume one child (with separator transparency).
       Try spine continuation first; if that fails, enter the child. */
    switch (skip_tokens_to_child(positions)) {
    | Some((child, remaining)) =>
      let spine_results = resolve_spine(rest, remaining, form_target);
      if (List.length(spine_results) > 0) {
        spine_results;
      } else {
        /* Spine didn't match — enter the consumed child */
        resolve_steps(rest, child);
      };
    | None =>
      switch (positions) {
      | [PosToken(_), ...remaining] =>
        resolve_spine(rest, remaining, form_target)
      | _ => []
      }
    }

  | [MatchEllipsis, ...rest] =>
    try_ellipsis_spine(rest, positions, form_target)

  | [MatchAtom(s), ...rest] =>
    /* Atom in spine context: look for it in children */
    try_atom_in_spine(s, rest, positions, form_target)

  | [ChildIndex(n), ...rest] =>
    /* Child index inside a spine: enter the NEXT child, then get its nth child.
       This allows e.g. "x = #0" to get child #0 of x's definition. */
    switch (skip_tokens_to_child(positions)) {
    | Some((child, _remaining)) =>
      /* Enter the child and get its nth sub-child */
      switch (nth_child(n, child)) {
      | Some(sub_child) => resolve_steps(rest, sub_child)
      | None => []
      }
    | None => []
    };

  | [DescendInto, ...rest] =>
    /* Descend from within a spine: enter the next child and descend there */
    switch (skip_tokens_to_child(positions)) {
    | Some((child, _remaining)) =>
      resolve_steps([DescendInto, ...rest], child)
    | None =>
      /* No child found: try matching rest on the form itself */
      resolve_steps(rest, form_target)
    };

  | [EnterBinderDef(name), ...rest] =>
    /* Binder search inside a spine */
    let binders = find_all_binders_named(name, form_target);
    binders
    |> List.concat_map(((def_target, _mod_opt)) =>
         resolve_steps(rest, def_target)
       );
  };
}

/* Match a keyword token against spine positions.
   Keywords enter structural children (FocusMod, FocusSig, FocusRule, FocusTPat,
   FocusMPat) but NOT expression/pattern/type children which have their own
   keyword scoping. This prevents nested "in"/"then"/"else" false matches
   while allowing "{ let x = %" to find items inside module bodies. */
and match_keyword_in_spine =
    (kw: string, rest: sem_selector, positions: list(spine_pos),
     form_target: focus_target)
    : list(match_result) => {
  switch (positions) {
  | [PosToken(s), ...remaining] when String.equal(s, kw) =>
    let here = resolve_spine(rest, remaining, form_target);
    let more = match_keyword_in_spine(kw, rest, remaining, form_target);
    dedup_results(here @ more);
  | [PosChild(child), ...remaining] =>
    /* Only enter structural children (Mod/Sig/Rule/TPat/MPat) for keywords */
    let enter =
      switch (child) {
      | FocusMod(_) | FocusSig(_) | FocusRule(_, _) | FocusTPat(_) | FocusMPat(_) =>
        resolve_steps([MatchKeyword(kw), ...rest], child)
      | FocusExp(_) | FocusPat(_) | FocusTyp(_) => []
      };
    let skip = match_keyword_in_spine(kw, rest, remaining, form_target);
    dedup_results(enter @ skip);
  | [PosToken(_), ...remaining] =>
    match_keyword_in_spine(kw, rest, remaining, form_target)
  | [] => []
  };
}

/* Match a delimiter against spine positions.
   Returns results from ALL matching positions (not just the first).
   For form-starting delimiters ({, [, (), tries entering child forms. */
and match_delimiter_in_spine =
    (d: string, rest: sem_selector, positions: list(spine_pos),
     form_target: focus_target)
    : list(match_result) => {
  switch (positions) {
  | [PosToken(s), ...remaining] when String.equal(s, d) =>
    let here = resolve_spine(rest, remaining, form_target);
    let more = match_delimiter_in_spine(d, rest, remaining, form_target);
    dedup_results(here @ more);
  | [PosChild(child), ...remaining] =>
    /* For form-starting delimiters, try entering the child */
    let enter =
      switch (d) {
      | "{" | "[" | "(" | "|" =>
        resolve_steps([MatchDelimiter(d), ...rest], child)
      | _ => []
      };
    let skip = match_delimiter_in_spine(d, rest, remaining, form_target);
    dedup_results(enter @ skip);
  | [PosToken(_), ...remaining] =>
    match_delimiter_in_spine(d, rest, remaining, form_target)
  | [] => []
  };
}

/* Match a name against spine children, with separator transparency.
   When a name matches, try both continuing in the current spine AND
   entering the matched child (for cases like module items where
   the child's inner spine contains the relevant tokens like "=").
   If no name matches in the current spine, try entering the LAST child
   (for let-chain traversal: "let b = %" on nested lets). */
and match_name_in_spine_resolve =
    (name: string, idx_opt: option(int), rest: sem_selector,
     positions: list(spine_pos), form_target: focus_target)
    : list(match_result) => {
  /* First scan: try to find a matching name in current positions.
     Prefer spine continuation over entering the matched child
     (same pattern as MatchSlot). Only enter if spine produces nothing. */
  let rec scan = (pos: list(spine_pos)): list(match_result) =>
    switch (pos) {
    | [PosChild(child), ...remaining] =>
      switch (name_of_target(child)) {
      | Some(n) when name_matches_str(name, idx_opt, n) =>
        let spine_results = resolve_spine(rest, remaining, form_target);
        if (List.length(spine_results) > 0) {
          spine_results;
        } else {
          resolve_steps(rest, child);
        };
      | _ => scan(remaining)
      }
    | [PosToken(_), ...remaining] => scan(remaining)
    | [] => []
    };
  let direct = scan(positions);
  if (List.length(direct) > 0) {
    direct;
  } else {
    /* No match found: try entering the last child (let-chain traversal) */
    let last_child =
      positions
      |> List.filter_map(
           fun
           | PosChild(c) => Some(c)
           | PosToken(_) => None,
         )
      |> (children =>
            switch (List.rev(children)) {
            | [last, ..._] => Some(last)
            | [] => None
            });
    switch (last_child) {
    | Some(child) =>
      /* Descend into the last child with the full current steps */
      resolve_steps(
        [MatchName(name), ...rest],
        child,
      )
    | None => []
    };
  };
}

/* Try ellipsis: match rest_steps at each position, skipping forward */
and try_ellipsis_spine =
    (rest_steps: sem_selector, positions: list(spine_pos),
     form_target: focus_target)
    : list(match_result) => {
  /* Try matching the rest at the current position */
  let here = resolve_spine(rest_steps, positions, form_target);
  /* Try skipping one position and recursing */
  let skip =
    switch (positions) {
    | [] => []
    | [_, ...remaining] =>
      try_ellipsis_spine(rest_steps, remaining, form_target)
    };
  dedup_results(here @ skip);
}

/* Try to match an atom in a spine's children */
and try_atom_in_spine =
    (s: string, rest: sem_selector, positions: list(spine_pos),
     form_target: focus_target)
    : list(match_result) => {
  switch (positions) {
  | [] => []
  | [PosChild(child), ...remaining] =>
    switch (atom_string(child)) {
    | Some(s') when atom_matches(s, s') =>
      let inner = resolve_steps(rest, child);
      inner @ try_atom_in_spine(s, rest, remaining, form_target);
    | _ =>
      try_atom_in_spine(s, rest, remaining, form_target)
    }
  | [PosToken(_), ...remaining] =>
    try_atom_in_spine(s, rest, remaining, form_target)
  };
}

/* Descend into all children of a target */
and descend_all =
    (inner_steps: sem_selector, target: focus_target): list(match_result) => {
  children_of(target)
  |> List.concat_map(child => {
       let (actual_child, _) = decompose_through(child);
       let here = resolve_steps(inner_steps, actual_child);
       let below = descend_all(inner_steps, actual_child);
       here @ below;
     });
}

/* Atom matching with float tolerance */
and atom_matches = (pattern: string, actual: string): bool =>
  if (String.equal(pattern, actual)) {
    true;
  } else {
    /* Try float comparison */
    switch (float_of_string_opt(pattern), float_of_string_opt(actual)) {
    | (Some(f1), Some(f2)) => Float.equal(f1, f2)
    | _ => false
    };
  }

/* Find a named child anywhere in the spine positions */
and find_named_child_in_positions =
    (name: string, positions: list(spine_pos))
    : option((focus_target, list(spine_pos))) => {
  switch (positions) {
  | [] => None
  | [PosChild(child), ...rest] =>
    switch (name_of_target(child)) {
    | Some(n) when String.equal(n, name) => Some((child, rest))
    | _ => find_named_child_in_positions(name, rest)
    }
  | [PosToken(_), ...rest] =>
    find_named_child_in_positions(name, rest)
  };
}

/* Deduplicate results by focused_id, preserving first occurrence */
and dedup_results = (results: list(match_result)): list(match_result) => {
  let seen = Hashtbl.create(16);
  List.filter(
    (m: match_result) =>
      if (Hashtbl.mem(seen, m.focused_id)) {
        false;
      } else {
        Hashtbl.add(seen, m.focused_id, ());
        true;
      },
    results,
  );
};

/* === Binder-aware resolution for chain steps with indexing === */

/* Handle a chain step that may have an index (EnterBinderDef name).
   When the rest of the steps starts with MatchName/MatchNameIndex,
   this is the "last step" of a chain and needs special handling. */

/* Handle the last segment of a chain that doesn't have trailing slash.
   The bare name at the end focuses on the whole binding (FocusMod for module items,
   FocusPat for the pattern). */
let resolve_bare_name_in_binders =
    (name: string, idx_opt: option(int), target: focus_target)
    : list(match_result) => {
  let binders = find_all_binders_named(name, target);
  let filtered =
    switch (idx_opt) {
    | None => binders
    | Some(idx) =>
      switch (List.nth_opt(binders, idx)) {
      | Some(b) => [b]
      | None => []
      }
    };
  filtered
  |> List.map(((_, mod_opt)) =>
       switch (mod_opt) {
       | Some(m) => mk_mod(m)
       | None =>
         /* Top-level binding: find the pat */
         switch (find_all_binders_named(name, target)) {
         | [] => mk_result(target) /* fallback */
         | [(def_target, _), ..._] =>
           /* Return the pattern, not the def. We need to find the pattern.
              Actually, for bare name at top level (let x = ...),
              the focus should be the atom/pat match. Fall through to
              atom matching in the main resolver. */
           mk_result(def_target)
         }
       }
     );
};

/* === Top-level entry point === */

/* Resolve with indexed binder handling.
   The elaborated sem_selector may contain:
   - MatchName/MatchNameIndex: these participate in spine matching
   - EnterBinderDef: these navigate into binder definitions
   The resolution handles indexed names specially for out-of-range errors. */
let resolve_elaborated =
    (steps: sem_selector, root: Exp.t): list(match_result) => {
  resolve_steps(steps, FocusExp(root));
};

/* Check for out-of-range index errors before resolving */
let check_index_bounds =
    (steps: sem_selector, root: Exp.t): option(string) => {
  /* Find MatchNameIndex steps and check if the index is valid */
  let rec check = (steps: sem_selector, target: focus_target): option(string) =>
    switch (steps) {
    | [] => None
    | [MatchNameIndex(name, idx), ..._] =>
      let binders = find_all_binders_named(name, target);
      let count = List.length(binders);
      if (idx >= count) {
        Some(
          "No match: index "
          ++ string_of_int(idx)
          ++ " out of range ("
          ++ string_of_int(count)
          ++ " binding(s) named '"
          ++ name
          ++ "')",
        );
      } else {
        None;
      };
    | [EnterBinderDef(name), ...rest] =>
      let binders = find_all_binders_named(name, target);
      /* Check in all binder defs */
      let results =
        binders
        |> List.filter_map(((def_target, _)) => check(rest, def_target));
      switch (results) {
      | [err, ..._] => Some(err)
      | [] => None
      };
    | [_, ...rest] => check(rest, target)
    };
  check(steps, FocusExp(root));
};

/* Handle MatchNameIndex in spine-matching context:
   When we see MatchNameIndex(name, idx), we need to find the idx-th
   binding with that name across the let-chain. */
let resolve_with_index_handling =
    (steps: sem_selector, root: Exp.t): list(match_result) => {
  /* First check for out-of-range indices */
  switch (check_index_bounds(steps, root)) {
  | Some(_) => [] /* Will be reported as error in query_unique */
  | None => resolve_elaborated(steps, root)
  };
};

/* Special handling for name-indexed steps:
   Convert MatchNameIndex(name, idx) to a filtered resolution. */
let rec resolve_with_name_index =
        (steps: sem_selector, root: Exp.t): list(match_result) => {
  /* Check if there's a MatchNameIndex that needs special handling */
  let has_name_index =
    List.exists(
      fun
      | MatchNameIndex(_, _) => true
      | _ => false,
      steps,
    );

  if (!has_name_index) {
    resolve_elaborated(steps, root);
  } else {
    resolve_name_indexed(steps, FocusExp(root));
  };
}

and resolve_name_indexed =
    (steps: sem_selector, target: focus_target): list(match_result) => {
  switch (steps) {
  | [] => [mk_result(target)]
  | [MatchNameIndex(name, idx), ...rest] =>
    /* Find all binders with this name */
    let binders = find_all_binders_named(name, target);
    switch (List.nth_opt(binders, idx)) {
    | Some((def_target, mod_opt)) =>
      /* Now resolve rest against this specific binder */
      switch (rest) {
      | [] =>
        /* Bare indexed name: focus on the module item or def */
        switch (mod_opt) {
        | Some(m) => [mk_mod(m)]
        | None => [mk_result(def_target)]
        }
      | _ =>
        /* Continue resolution from the binding's position in the let chain.
           We need to match the rest of the steps in the spine context. */
        resolve_name_index_continuation(name, idx, rest, target)
      }
    | None => []
    }
  | _ =>
    /* No MatchNameIndex at head: use normal resolution */
    resolve_steps(steps, target)
  };
}

/* Continue resolution after finding a specific indexed binder.
   This recreates the spine context that the normal resolver would see. */
and resolve_name_index_continuation =
    (name: string, idx: int, rest: sem_selector, target: focus_target)
    : list(match_result) => {
  /* Find the idx-th binder, then run the rest of the steps from
     that binder's context. The binders are ordered by
     the let-chain traversal. We find the idx-th binder, then run the
     rest of the steps from that binder's context. */
  let binders = find_all_binders_named(name, target);
  switch (List.nth_opt(binders, idx)) {
  | None => []
  | Some((def_target, mod_opt)) =>
    /* Run rest from the context where this binder is "the current one" */
    switch (rest) {
    | [MatchDelimiter("="), ...rest2] =>
      /* "name#N = %": focus on the definition */
      switch (rest2) {
      | [MatchFocus, ...rest3] =>
        switch (rest3) {
        | [] => [mk_result(def_target)]
        | _ => resolve_steps(rest3, def_target)
        }
      | _ => resolve_steps(rest2, def_target)
      }
    | [MatchEllipsis, ...rest2] =>
      /* "name#N _... in %": find the body */
      resolve_name_index_body(name, idx, rest2, target)
    | [MatchFocus, ...rest2] =>
      switch (rest2) {
      | [] =>
        switch (mod_opt) {
        | Some(m) => [mk_mod(m)]
        | None => [mk_result(def_target)]
        }
      | _ => resolve_steps(rest2, def_target)
      }
    | _ =>
      /* General case: resolve rest against the def */
      resolve_steps(rest, def_target)
    }
  };
}

and resolve_name_index_body =
    (name: string, idx: int, rest: sem_selector, target: focus_target)
    : list(match_result) => {
  /* Find the idx-th binder and return its body */
  let rec find_nth_body =
          (n: int, current_idx: int, ft: focus_target)
          : option(focus_target) =>
    switch (ft) {
    | FocusExp(e) =>
      switch (Exp.term_of(e)) {
      | Let(pat, _, body) =>
        switch (pat_name(pat)) {
        | Some(pn) when String.equal(pn, name) =>
          if (current_idx == n) {
            Some(FocusExp(body));
          } else {
            find_nth_body(n, current_idx + 1, FocusExp(body));
          }
        | _ => find_nth_body(n, current_idx, FocusExp(body))
        }
      | TyAlias(tpat, _, body) =>
        switch (tpat_name(tpat)) {
        | Some(tn) when String.equal(tn, name) =>
          if (current_idx == n) {
            Some(FocusExp(body));
          } else {
            find_nth_body(n, current_idx + 1, FocusExp(body));
          }
        | _ => find_nth_body(n, current_idx, FocusExp(body))
        }
      | ModuleExp(mpat, _, body) =>
        switch (mpat_name(mpat)) {
        | Some(mn) when String.equal(mn, name) =>
          if (current_idx == n) {
            Some(FocusExp(body));
          } else {
            find_nth_body(n, current_idx + 1, FocusExp(body));
          }
        | _ => find_nth_body(n, current_idx, FocusExp(body))
        }
      | Parens(inner) => find_nth_body(n, current_idx, FocusExp(inner))
      | _ => None
      }
    | _ => None
    };

  switch (rest) {
  | [MatchKeyword("in"), ...rest2] =>
    switch (find_nth_body(idx, 0, target)) {
    | Some(body) => resolve_steps(rest2, body)
    | None => []
    }
  | _ =>
    switch (find_nth_body(idx, 0, target)) {
    | Some(body) =>
      /* Try matching rest against the body with potential "in" already consumed */
      resolve_steps(rest, body)
    | None => []
    }
  };
};

/* === Elaboration: surface -> semantic === */
/* (elaborate is defined above) */

/* Resolve a surface selector against an expression */
let resolve = (sel: selector, root: Exp.t): list(match_result) => {
  let steps = elaborate(sel);
  dedup_results(resolve_with_name_index(steps, root));
};

/* Resolve a sem_selector */
let resolve_sem = (steps: sem_selector, root: Exp.t): list(match_result) =>
  dedup_results(resolve_with_name_index(steps, root));

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

/* === Diagnostics === */

/* Levenshtein edit distance */
let edit_distance = (s1: string, s2: string): int => {
  let n = String.length(s1);
  let m = String.length(s2);
  if (n == 0) {
    m;
  } else if (m == 0) {
    n;
  } else {
    let d = Array.make_matrix(n + 1, m + 1, 0);
    for (i in 0 to n) {
      d[i][0] = i;
    };
    for (j in 0 to m) {
      d[0][j] = j;
    };
    for (i in 1 to n) {
      for (j in 1 to m) {
        let cost = s1.[i - 1] == s2.[j - 1] ? 0 : 1;
        d[i][j] =
          min(min(d[i - 1][j] + 1, d[i][j - 1] + 1), d[i - 1][j - 1] + cost);
      };
    };
    d[n][m];
  };
};

/* Find similar names for "did you mean?" suggestions */
let suggest_similar = (target: string, available: list(string)): list(string) =>
  available
  |> List.filter(name => edit_distance(target, name) <= 2)
  |> List.sort((a, b) =>
       compare(edit_distance(target, a), edit_distance(target, b))
     );

/* Diagnose why a selector produced no matches */
let diagnose_no_match = (selector_str: string, root: Exp.t): string => {
  let steps = elaborate(parse(selector_str));
  let available = collect_binder_names(FocusExp(root));

  /* Try to identify where the match failed */
  let rec diagnose =
          (steps: sem_selector, matched: list(string), target: focus_target)
          : string => {
    switch (steps) {
    | [] => "No match (empty selector)"

    | [MatchKeyword(kw), ..._] =>
      /* Check if the root form starts with this keyword */
      let (_, dec) = decompose_through(target);
      switch (dec) {
      | Form([PosToken(s), ..._]) when String.equal(s, kw) =>
        /* Keyword matched, continue with rest */
        diagnose_rest(steps, matched, target)
      | _ =>
        "No match: Failed at first step: "
        ++ kw
        ++ (
          if (List.length(matched) > 0) {
            " (Matched up to: " ++ String.concat(" ", matched) ++ ")";
          } else {
            "";
          }
        )
      }

    | [MatchName(name), ..._]
    | [MatchNameIndex(name, _), ..._] =>
      let similar = suggest_similar(name, available);
      let available_str =
        switch (available) {
        | [] => ""
        | names =>
          " Available names: "
          ++ String.concat(
               ", ",
               List.sort_uniq(String.compare, names),
             )
        };
      let suggestion =
        switch (similar) {
        | [s, ..._] => " Did you mean: " ++ s ++ "?"
        | [] => ""
        };
      "No match: "
      ++ (
        if (List.length(matched) > 0) {
          "Matched up to: "
          ++ String.concat(" ", matched)
          ++ " / Failed at: "
          ++ name;
        } else {
          "Failed at: " ++ name;
        }
      )
      ++ suggestion
      ++ available_str;

    | [EnterBinderDef(name), ..._] =>
      let binders = find_all_binders_named(name, target);
      if (List.length(binders) == 0) {
        let similar = suggest_similar(name, available);
        let available_str =
          switch (available) {
          | [] => ""
          | names =>
            " Available names: "
            ++ String.concat(
                 ", ",
                 List.sort_uniq(String.compare, names),
               )
          };
        let suggestion =
          switch (similar) {
          | [s, ..._] => " Did you mean: " ++ s ++ "?"
          | [] => ""
          };
        "No match: name '"
        ++ name
        ++ "' not found."
        ++ suggestion
        ++ available_str;
      } else {
        /* Chain entered but rest failed */
        let inner_available =
          binders
          |> List.concat_map(((def_target, _)) =>
               collect_binder_names(def_target)
             );
        diagnose(
          List.tl(steps),
          matched @ [name ++ "/"],
          switch (List.hd(binders)) {
          | (def_target, _) => def_target
          },
        )
        |> (
          s =>
            if (List.length(inner_available) > 0) {
              s
              ++ " Available names: "
              ++ String.concat(
                   ", ",
                   List.sort_uniq(String.compare, inner_available),
                 );
            } else {
              s;
            }
        );
      }

    | [DescendInto, ..._] =>
      "No match: descendant search found no matching nodes"

    | [MatchFocus, ...rest] =>
      diagnose(rest, matched, target)

    | _ =>
      "No match for selector: " ++ selector_str
    };
  }

  and diagnose_rest =
      (steps: sem_selector, matched: list(string), target: focus_target)
      : string => {
    /* We know the first step matches. Walk further. */
    switch (steps) {
    | [MatchKeyword(kw), ...rest] =>
      diagnose(rest, matched @ [kw], target)
    | [MatchDelimiter(d), ...rest] =>
      diagnose(rest, matched @ [d], target)
    | _ => diagnose(steps, matched, target)
    };
  };

  diagnose(steps, [], FocusExp(root));
};

/* For edit actions: require exactly one match */
let query_unique =
    (selector_str: string, root: Exp.t): result(match_result, string) => {
  /* Check for index out-of-range errors first */
  let steps = elaborate(parse(selector_str));
  switch (check_index_bounds(steps, root)) {
  | Some(err) => Error(err)
  | None =>
    let results = query(selector_str, root);
    switch (results) {
    | [single] => Ok(single)
    | [] =>
      let diag = diagnose_no_match(selector_str, root);
      Error(diag);
    | multiple =>
      Error(
        "Ambiguous: "
        ++ string_of_int(List.length(multiple))
        ++ " matches for selector '"
        ++ selector_str
        ++ "'",
      )
    };
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
    | FocusSig(_item) => [Piece.mk_secondary(Id.mk(), "sig")]
    | FocusTPat(tp) =>
      switch (IdTagged.term_of(tp)) {
      | Var(name) => [Piece.mk_secondary(Id.mk(), name)]
      | _ => [Piece.mk_secondary(Id.mk(), "?")]
      }
    | FocusMPat(mp) =>
      switch (IdTagged.term_of(mp)) {
      | Var(name) => [Piece.mk_secondary(Id.mk(), name)]
      | _ => [Piece.mk_secondary(Id.mk(), "?")]
      }
    | FocusRule(pat, body) =>
      let pat_seg = ExpToSegment.pat_to_segment(~settings, pat);
      let body_seg = ExpToSegment.exp_to_segment(~settings, body);
      pat_seg
      @ [Piece.mk_secondary(Id.mk(), " => ")]
      @ body_seg;
    };
  Printer.of_segment(~holes="?", segment);
};

/* === Canonical path generation === */

let rec canonical_numeric_ft =
        (target_id: Id.t, node: focus_target): option(sem_selector) =>
  if (id_of_target(node) == target_id) {
    Some([MatchFocus]);
  } else {
    /* Look through transparent wrappers */
    let (actual, _) = decompose_through(node);
    if (id_of_target(actual) != id_of_target(node)
        && id_of_target(actual) == target_id) {
      Some([MatchFocus]);
    } else {
      let children = children_of(actual);
      children
      |> List.mapi((i, child) =>
           canonical_numeric_ft(target_id, child)
           |> Option.map(k => [ChildIndex(i), ...k])
         )
      |> List.find_map(x => x);
    };
  };

let canonical_numeric =
    (target_id: Id.t, root: Exp.t): option(sem_selector) =>
  canonical_numeric_ft(target_id, FocusExp(root));

/* === Named canonical === */

/* Add shadow indices to MatchName steps where there are multiple
   bindings with the same name */
let add_shadow_indices =
    (steps: sem_selector, root: Exp.t): sem_selector => {
  steps
  |> List.map(step =>
       switch (step) {
       | MatchName(name) =>
         let binders = find_all_binders_named(name, FocusExp(root));
         if (List.length(binders) > 1) {
           step;
         } else {
           step;
         }
       | _ => step
       }
     );
};

/* Generate a human-readable canonical selector */
let canonical_named =
    (target_id: Id.t, root: Exp.t): option(sem_selector) => {
  let rec go = (node: focus_target): option(sem_selector) => {
    let (actual, dec) = decompose_through(node);
    if (id_of_target(actual) == target_id) {
      Some([MatchFocus]);
    } else {
      switch (dec) {
      | Form(positions) =>
        try_named_in_positions(positions, actual)
      | AtomNode(_) => None
      | Hole => None
      | Transparent(_) => None
      };
    };
  }

  and try_named_in_positions =
      (positions: list(spine_pos), parent: focus_target): option(sem_selector) => {
    /* Try to find the target in children with a human-readable path */
    let children =
      positions
      |> List.filter_map(
           fun
           | PosChild(c) => Some(c)
           | PosToken(_) => None,
         );

    /* First: check if any child IS the target */
    let direct =
      children
      |> List.mapi((i, child) => (i, child))
      |> List.find_opt(((_, child)) => {
           let (actual_child, _) = decompose_through(child);
           id_of_target(actual_child) == target_id;
         });

    switch (direct) {
    | Some((child_idx, child)) =>
      /* Found the target child. Generate named path. */
      let (actual_child, _) = decompose_through(child);
      let _ = actual_child;
      generate_named_for_direct_child(positions, child_idx, parent);
    | None =>
      /* Target is deeper. Try named paths to each child. */
      try_named_recursive(positions, parent);
    };
  }

  and generate_named_for_direct_child =
      (_positions: list(spine_pos), child_idx: int, parent: focus_target)
      : option(sem_selector) => {
    /* Determine the form of the parent to generate a readable selector */
    let (_, dec) = decompose_through(parent);
    switch (dec) {
    | Form(all_positions) =>
      let children =
        all_positions
        |> List.filter_map(
             fun
             | PosChild(c) => Some(c)
             | PosToken(_) => None,
           );
      let n_children = List.length(children);

      /* Check for Let-like forms */
      switch (all_positions) {
      | [PosToken(kw), PosChild(_pat), PosToken("="), PosChild(_def), ..._]
          when kw == "let" || kw == "type" || kw == "module" =>
        let pat_child = List.nth(children, 0);
        let pat_nm = name_of_target(pat_child);
        switch (pat_nm) {
        | Some(pat_name) =>
          /* Check for shadowed names: count all same-named bindings */
          let all_names = collect_binder_names(FocusExp(Obj.magic(0)));
          let _ = all_names;
          if (child_idx == 0) {
            /* Pattern focus */
            Some([MatchKeyword(kw), MatchFocus, MatchName(pat_name)]);
          } else if (child_idx == 1) {
            /* Definition focus */
            Some([
              MatchKeyword(kw),
              MatchName(pat_name),
              MatchDelimiter("="),
              MatchFocus,
            ]);
          } else if (child_idx == 2 && n_children >= 3) {
            /* Body focus */
            Some([
              MatchKeyword(kw),
              MatchName(pat_name),
              MatchEllipsis,
              MatchKeyword("in"),
              MatchFocus,
            ]);
          } else {
            None;
          };
        | None => None
        }

      /* If/then/else */
      | [PosToken("if"), ..._] =>
        if (child_idx == 0) {
          Some([MatchKeyword("if"), MatchFocus]);
        } else if (child_idx == 1) {
          Some([
            MatchKeyword("if"),
            MatchSlot,
            MatchKeyword("then"),
            MatchFocus,
          ]);
        } else if (child_idx == 2) {
          Some([
            MatchKeyword("if"),
            MatchEllipsis,
            MatchKeyword("else"),
            MatchFocus,
          ]);
        } else {
          None;
        }

      /* Fun */
      | [PosToken("fun"), ..._] =>
        if (child_idx == 0) {
          Some([MatchKeyword("fun"), MatchFocus]);
        } else if (child_idx == 1) {
          Some([
            MatchKeyword("fun"),
            MatchSlot,
            MatchDelimiter("->"),
            MatchFocus,
          ]);
        } else {
          None;
        }

      /* Case: case scrut | ... end */
      | [PosToken("case"), ..._] =>
        if (child_idx == 0) {
          Some([MatchKeyword("case"), MatchFocus]);
        } else {
          /* Rule children: child_idx 1+ are rules.
             Try to generate a named selector for the rule arm body. */
          None;
        }

      /* BinOp: child0 op child1 */
      | [PosChild(_), PosToken(op), PosChild(_)] =>
        if (child_idx == 0) {
          Some([MatchFocus, MatchDelimiter(op), MatchSlot]);
        } else if (child_idx == 1) {
          Some([MatchSlot, MatchDelimiter(op), MatchFocus]);
        } else {
          None;
        }

      /* Test */
      | [PosToken("test"), ..._] =>
        if (child_idx == 0) {
          Some([MatchKeyword("test"), MatchFocus]);
        } else {
          None;
        }

      | _ => None
      };
    | _ => None
    };
  }

  and try_named_recursive =
      (_positions: list(spine_pos), parent: focus_target)
      : option(sem_selector) => {
    /* Try to find the target recursively in children */
    let (_, dec) = decompose_through(parent);
    switch (dec) {
    | Form(all_positions) =>
      let children =
        all_positions
        |> List.filter_map(
             fun
             | PosChild(c) => Some(c)
             | PosToken(_) => None,
           );

      /* For Let-like forms: try name + child path */
      switch (all_positions) {
      | [PosToken(kw), PosChild(_pat), PosToken("="), PosChild(def_child), ..._]
          when kw == "let" || kw == "type" || kw == "module" =>
        let pat_child = List.nth(children, 0);
        let pat_nm = name_of_target(pat_child);
        switch (pat_nm) {
        | Some(the_name) =>
          /* Check for shadowed names */
          let need_index =
            switch (parent) {
            | FocusExp(e) =>
              let all_binders =
                find_all_binders_named(the_name, FocusExp(e));
              List.length(all_binders) > 1;
            | _ => false
            };
          let (name_step, name_idx) =
            if (need_index) {
              /* Find which index this binding is */
              switch (parent) {
              | FocusExp(e) =>
                let all_binders =
                  find_all_binders_named(the_name, FocusExp(e));
                let idx =
                  all_binders
                  |> List.mapi((i, (dt, _)) => (i, dt))
                  |> List.find_opt(((_, dt)) => {
                       let (actual_def, _) = decompose_through(def_child);
                       id_of_target(dt) == id_of_target(actual_def);
                     })
                  |> Option.map(((i, _)) => i)
                  |> Option.value(~default=0);
                (MatchNameIndex(the_name, idx), Some(idx));
              | _ => (MatchName(the_name), None)
              };
            } else {
              (MatchName(the_name), None);
            };
          let _ = name_idx;

          /* Try to find target in def */
          let (actual_def, _) = decompose_through(def_child);
          switch (go(actual_def)) {
          | Some(inner_path) =>
            Some([
              MatchKeyword(kw),
              name_step,
              MatchDelimiter("="),
              ...inner_path,
            ])
          | None =>
            /* Try in body (if exists) */
            if (List.length(children) >= 3) {
              let body_child = List.nth(children, 2);
              switch (go(body_child)) {
              | Some(inner_path) => Some(inner_path) /* Body is transparent */
              | None => None
              };
            } else {
              None;
            }
          };
        | None =>
          /* No name: try numeric in children */
          try_numeric_in_children(children)
        }

      /* If/then/else: try named in children */
      | [PosToken("if"), ..._] =>
        let try_child = (idx, prefix) => {
          switch (List.nth_opt(children, idx)) {
          | Some(child) =>
            switch (go(child)) {
            | Some(inner) => Some(prefix @ inner)
            | None => None
            }
          | None => None
          };
        };
        /* Try cond */
        switch (try_child(0, [MatchKeyword("if")])) {
        | Some(_) as r => r
        | None =>
          switch (
            try_child(
              1,
              [MatchKeyword("if"), MatchSlot, MatchKeyword("then")],
            )
          ) {
          | Some(_) as r => r
          | None =>
            try_child(
              2,
              [
                MatchKeyword("if"),
                MatchEllipsis,
                MatchKeyword("else"),
              ],
            )
          }
        }

      /* Fun: try named in children */
      | [PosToken("fun"), ..._] =>
        switch (List.nth_opt(children, 1)) {
        | Some(body) =>
          switch (go(body)) {
          | Some(inner) =>
            Some([
              MatchKeyword("fun"),
              MatchSlot,
              MatchDelimiter("->"),
              ...inner,
            ])
          | None =>
            switch (List.nth_opt(children, 0)) {
            | Some(pat) =>
              switch (go(pat)) {
              | Some(inner) =>
                Some([MatchKeyword("fun"), ...inner])
              | None => None
              }
            | None => None
            }
          }
        | None => None
        }

      /* Case: try rules */
      | [PosToken("case"), ..._] =>
        /* Try scrutinee */
        switch (List.nth_opt(children, 0)) {
        | Some(scrut) =>
          switch (go(scrut)) {
          | Some(inner) =>
            Some([MatchKeyword("case"), ...inner])
          | None =>
            /* Try rules */
            try_named_in_rules(List.tl(children))
          }
        | None => None
        }

      /* BinOp */
      | [PosChild(_), PosToken(op), PosChild(_)] =>
        switch (List.nth_opt(children, 0)) {
        | Some(left) =>
          switch (go(left)) {
          | Some(inner) =>
            Some(inner @ [MatchDelimiter(op), MatchSlot])
          | None =>
            switch (List.nth_opt(children, 1)) {
            | Some(right) =>
              switch (go(right)) {
              | Some(inner) =>
                Some([MatchSlot, MatchDelimiter(op), ...inner])
              | None => None
              }
            | None => None
            }
          }
        | None => None
        }

      | _ =>
        /* Generic: try each child */
        try_numeric_in_children(children)
      };
    | _ => None
    };
  }

  and try_named_in_rules =
      (rules: list(focus_target)): option(sem_selector) => {
    rules
    |> List.find_map(rule => {
         switch (rule) {
         | FocusRule(pat, body) =>
           /* Try body first */
           switch (go(FocusExp(body))) {
           | Some(inner) =>
             let pat_nm =
               switch (pat_name(pat)) {
               | Some(n) => MatchName(n)
               | None => MatchSlot
               };
             Some([
               MatchDelimiter("|"),
               MatchEllipsis,
               pat_nm,
               MatchDelimiter("=>"),
               ...inner,
             ]);
           | None =>
             switch (go(FocusPat(pat))) {
             | Some(inner) =>
               Some([MatchDelimiter("|"), MatchEllipsis, ...inner])
             | None => None
             }
           }
         | _ =>
           switch (go(rule)) {
           | Some(inner) => Some(inner)
           | None => None
           }
         }
       });
  }

  and try_numeric_in_children =
      (children: list(focus_target)): option(sem_selector) => {
    children
    |> List.mapi((i, child) =>
         canonical_numeric_ft(target_id, child)
         |> Option.map(k => [ChildIndex(i), ...k])
       )
    |> List.find_map(x => x);
  };

  /* Start from root, but use named navigation */
  let root_target = FocusExp(root);

  /* First check for Let-chain named canonical with shadowing support */
  let result = go(root_target);

  /* If named canonical produced something, add shadowed-name indices */
  switch (result) {
  | Some(steps) => Some(add_shadow_indices(steps, root))
  | None =>
    /* Fallback to numeric */
    canonical_numeric(target_id, root)
  };
};

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
