open Language;

/* Selector language for addressing Hazel syntax subtrees.
      See plans/Hazel-Agent-Path-Selector-Language.md for the full spec.

      Core operators:
      - `_`    : matches one syntactic slot
      - `_...` : matches zero or more slots along current spine
      - `⋱`/`\...` : descendant search (match P, then find Q inside)
      - `*`    : focus marker (selects the next syntactic unit)

      Binder-chain sugar:
      - `A/B/C` : navigate into binder A's def, then B's def, then resolve C
   */

/* === Surface AST === */

/* A token in the selector surface syntax */
type token =
  | Slot /* _ */
  | Ellipsis /* _... or … */
  | Focus /* * */
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
  | Chain(list(string), bool) /* A/B/C - binder chain; bool = trailing slash */
  | Name(string) /* bare name */
  | NameIndex(string, int); /* x#0, x#1 - indexed name for shadowed bindings */

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
  | DescendInto /* descend into matched subtree */
  | EnterBinderDef(string); /* find binder by name, enter its def */

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

/* === Tokenizer / Parser === */

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
      | "*" => Focus
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
        /* name#N syntax for indexed disambiguation */
        let parts = String.split_on_char('#', s);
        switch (parts) {
        | [name, idx_str] when String.length(name) > 0 =>
          switch (int_of_string_opt(idx_str)) {
          | Some(idx) => NameIndex(name, idx)
          | None => Name(s) /* fallback if not a number */
          }
        | _ => Name(s)
        };
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
    | [NameIndex(name, idx), ...rest] => [MatchNameIndex(name, idx), ...go(rest)]
    | [Name(s), ...rest] => [MatchName(s), ...go(rest)]
    };
  let steps = go(sel);
  /* Implicit star: if no MatchFocus in the selector, append one.
     This means selectors like `A/B/C/` or `let x` produce a result
     without requiring an explicit `*`. */
  if (List.exists(s => s == MatchFocus, steps)) {
    steps;
  } else {
    steps @ [MatchFocus];
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
    here @ find_all_binders_named(name, body)
  | ModuleExp(mpat, def, body) =>
    let here =
      switch (mpat_name(mpat)) {
      | Some(n) when String.equal(n, name) => [(def, body)]
      | _ => []
      };
    here @ find_all_binders_named(name, body)
  | TyAlias(_tpat, _tdef, body) =>
    find_all_binders_named(name, body)
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
let find_let_node_indexed =
        (name: string, idx: int, e: Exp.t): option(Exp.t) => {
  let rec collect = (e: Exp.t): list(Exp.t) =>
    switch (Exp.term_of(e)) {
    | Let(pat, _def, body) =>
      let here =
        switch (pat_name(pat)) {
        | Some(n) when String.equal(n, name) => [e]
        | _ => []
        };
      here @ collect(body)
    | ModuleExp(mpat, _def, body) =>
      let here =
        switch (mpat_name(mpat)) {
        | Some(n) when String.equal(n, name) => [e]
        | _ => []
        };
      here @ collect(body)
    | TyAlias(tpat, _tdef, body) =>
      let here =
        switch (tpat_name(tpat)) {
        | Some(n) when String.equal(n, name) => [e]
        | _ => []
        };
      here @ collect(body)
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

/* Resolve an elaborated semantic selector against an expression */
let resolve_sem = (steps: sem_selector, root: Exp.t): list(match_result) => {
  /* Walk the selector steps against a current expression context.
     Returns list of (focused_exp) for each match. */
  let rec walk = (steps: sem_selector, current: Exp.t): list(match_result) =>
    switch (steps) {
    | [] => []

    /* Focus: return the current expression as match */
    | [MatchFocus] => [
        {
          focused: FocusExp(current),
          focused_id: Exp.rep_id(current),
          breadcrumb: "",
        },
      ]

    /* Focus + more steps: focus on whatever the remaining steps select */
    | [MatchFocus, ...rest] => walk(rest, current)

    /* EnterBinderDef: find binder by name, enter its definition */
    | [EnterBinderDef(name), ...rest] =>
      switch (find_binder_in_exp(name, current)) {
      | Some((def, _body)) => walk(rest, def)
      | None => [] /* binder not found */
      }

    /* MatchName: find a binder by name in the current expression */
    | [MatchName(name)] =>
      /* If this is the final step with no focus, match the whole binding */
      switch (find_let_node(name, current)) {
      | Some(let_exp) => [
          {
            focused: FocusExp(let_exp),
            focused_id: Exp.rep_id(let_exp),
            breadcrumb: name,
          },
        ]
      | None =>
        /* Also check module items */
        switch (find_binder_in_exp(name, current)) {
        | Some((def, _)) => [
            {
              focused: FocusExp(def),
              focused_id: Exp.rep_id(def),
              breadcrumb: name,
            },
          ]
        | None => []
        }
      }

    /* name = * : select the definition of binder `name` */
    | [MatchName(name), MatchDelimiter("="), MatchFocus] =>
      switch (find_binder_in_exp(name, current)) {
      | Some((def, _body)) => [
          {
            focused: FocusExp(def),
            focused_id: Exp.rep_id(def),
            breadcrumb: name ++ " = ...",
          },
        ]
      | None => []
      }

    /* name = <more> : enter the definition of binder `name` */
    | [MatchName(name), MatchDelimiter("="), ...rest] =>
      switch (find_binder_in_exp(name, current)) {
      | Some((def, _body)) => walk(rest, def)
      | None => []
      }

    /* name ... in * : select the body of binder `name` */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
      switch (find_binder_in_exp(name, current)) {
      | Some((_def, body)) => [
          {
            focused: FocusExp(body),
            focused_id: Exp.rep_id(body),
            breadcrumb: name ++ " ... in ...",
          },
        ]
      | None => []
      }

    /* name ... in <more> : enter the body of binder `name` */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), ...rest] =>
      switch (find_binder_in_exp(name, current)) {
      | Some((_def, body)) => walk(rest, body)
      | None => []
      }

    | [MatchName(name), ...rest] =>
      /* Name followed by other steps: find the let node, continue */
      switch (find_let_node(name, current)) {
      | Some(let_exp) => walk(rest, let_exp)
      | None =>
        /* Try in module context */
        switch (find_binder_in_exp(name, current)) {
        | Some((def, _)) => walk(rest, def)
        | None => []
        }
      }

    /* MatchNameIndex: indexed disambiguation for shadowed bindings */
    | [MatchNameIndex(name, idx)] =>
      switch (find_let_node_indexed(name, idx, current)) {
      | Some(let_exp) => [
          {
            focused: FocusExp(let_exp),
            focused_id: Exp.rep_id(let_exp),
            breadcrumb: name ++ "#" ++ string_of_int(idx),
          },
        ]
      | None =>
        switch (find_binder_indexed(name, idx, current)) {
        | Some((def, _)) => [
            {
              focused: FocusExp(def),
              focused_id: Exp.rep_id(def),
              breadcrumb: name ++ "#" ++ string_of_int(idx),
            },
          ]
        | None => []
        }
      }

    | [MatchNameIndex(name, idx), MatchDelimiter("="), MatchFocus] =>
      switch (find_binder_indexed(name, idx, current)) {
      | Some((def, _body)) => [
          {
            focused: FocusExp(def),
            focused_id: Exp.rep_id(def),
            breadcrumb: name ++ "#" ++ string_of_int(idx) ++ " = ...",
          },
        ]
      | None => []
      }

    | [MatchNameIndex(name, idx), MatchDelimiter("="), ...rest] =>
      switch (find_binder_indexed(name, idx, current)) {
      | Some((def, _body)) => walk(rest, def)
      | None => []
      }

    | [MatchNameIndex(name, idx), MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
      switch (find_binder_indexed(name, idx, current)) {
      | Some((_def, body)) => [
          {
            focused: FocusExp(body),
            focused_id: Exp.rep_id(body),
            breadcrumb: name ++ "#" ++ string_of_int(idx) ++ " ... in ...",
          },
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
         LAYERING NOTE: This intercepts indexed names at the walk dispatch
         level rather than inside walk_let_spine. The cleaner approach would
         be to pass an index parameter through to spine walkers, but that
         would require adding ~idx:option(int) to walk_let_spine,
         walk_fun_spine, walk_after_module_kw, and walk_after_type_kw plus
         their name-matching branches. Current approach is contained here
         (~12 lines) and only affects let keyword. Generalize if/when
         indexing is needed for fun/module/type binders. */
      let matching_spines =
        find_all_lets(current)
        |> List.filter((spine: let_spine) =>
             Option.equal(String.equal, pat_name(spine.pat), Some(name))
           );
      switch (List.nth_opt(matching_spines, idx)) {
      | Some(spine) =>
        walk_let_spine(spine, [MatchName(name), ...after_name])
      | None => []
      }
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

    /* module keyword */
    | [MatchKeyword("module"), ...rest] =>
      switch (Exp.term_of(current)) {
      | ModuleExp(mpat, def, body) =>
        switch (mpat_name(mpat)) {
        | Some(_name) =>
          walk_after_module_kw(mpat_name(mpat), def, current, Some(body), None, rest)
        | None => []
        }
      | Let(pat, def, body) =>
        switch (Exp.term_of(def)) {
        | Module(_) =>
          switch (pat_name(pat)) {
          | Some(_) =>
            walk_after_module_kw(pat_name(pat), def, current, Some(body), None, rest)
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
                  mpat_name(mpat), def, current, None, Some(item), rest)
              | None => []
              }
            | _ => []
            },
          items,
        )
      | _ => []
      }

    /* type keyword */
    | [MatchKeyword("type"), ...rest] =>
      switch (Exp.term_of(current)) {
      | TyAlias(tpat, tdef, body) =>
        walk_after_type_kw(tpat, current, Some(body), Some(tdef), None, rest)
      | Module(items) =>
        /* Match ModType items inside a module body */
        List.concat_map(
          (item: Mod.t) =>
            switch (item.term) {
            | ModType(tpat, tdef) =>
              walk_after_type_kw(tpat, current, None, Some(tdef), Some(item), rest)
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
        walk_fun_spine({pat, body, whole: current}, rest)
      | _ => []
      }

    /* test keyword */
    | [MatchKeyword("test"), ...rest] =>
      switch (Exp.term_of(current)) {
      | Test(body) =>
        walk_test_spine({body, whole: current}, rest)
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

    /* Pipe: match a case arm with the given constructor */
    | [MatchDelimiter("|"), ...rest] => walk_pipe(current, rest)

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

    | _ => [] /* unhandled pattern */
    }

  /* Walk a let spine after "let" keyword */
  and walk_let_spine = (spine: let_spine, steps: sem_selector) =>
    switch (steps) {
    | [] => [
        {
          focused: FocusExp(spine.whole),
          focused_id: Exp.rep_id(spine.whole),
          breadcrumb: "",
        },
      ]
    | [MatchFocus] => [
        {
          focused: FocusExp(spine.whole),
          focused_id: Exp.rep_id(spine.whole),
          breadcrumb: "",
        },
      ]

    /* let <name> = * : focus on definition */
    | [MatchName(name), MatchDelimiter("="), MatchFocus]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        {
          focused: FocusExp(spine.def),
          focused_id: Exp.rep_id(spine.def),
          breadcrumb: "let " ++ name ++ " = ...",
        },
      ]

    /* let <name> = * ... : focus on definition, continue */
    | [MatchName(name), MatchDelimiter("="), MatchFocus, ...rest]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.def)

    /* let <name> ... in * : focus on body */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), MatchFocus]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        {
          focused: FocusExp(spine.body),
          focused_id: Exp.rep_id(spine.body),
          breadcrumb: "let " ++ name ++ " ... in ...",
        },
      ]

    /* let <name> ... in * <more> : focus on body, continue */
    | [
        MatchName(name),
        MatchEllipsis,
        MatchKeyword("in"),
        MatchFocus,
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

    /* Colon patterns: let <name> : * selects type annotation.
       In Hazel, `let x : T = def in body` parses as Let(Asc(pat, typ), def, body). */

    /* let <name> : * : focus on the type annotation */
    | [MatchName(name), MatchDelimiter(":"), MatchFocus] =>
      switch (Pat.term_of(spine.pat)) {
      | Asc(inner_pat, ty) when Option.equal(String.equal, Pat.is_var(inner_pat), Some(name)) =>
        [{
          focused: FocusTyp(ty),
          focused_id: Typ.rep_id(ty),
          breadcrumb: "let " ++ name ++ " : ...",
        }]
      | _ => []
      }

    /* let <name> : _ = * : skip type annotation, focus on def */
    | [MatchName(name), MatchDelimiter(":"), MatchSlot, MatchDelimiter("="), MatchFocus] =>
      switch (Pat.term_of(spine.pat)) {
      | Asc(inner_pat, _ty) when Option.equal(String.equal, Pat.is_var(inner_pat), Some(name)) =>
        [{
          focused: FocusExp(spine.def),
          focused_id: Exp.rep_id(spine.def),
          breadcrumb: "let " ++ name ++ " : _ = ...",
        }]
      /* Also handle non-Asc patterns — name : _ = * just skips to def */
      | _ when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
        [{
          focused: FocusExp(spine.def),
          focused_id: Exp.rep_id(spine.def),
          breadcrumb: "let " ++ name ++ " : _ = ...",
        }]
      | _ => []
      }

    /* let <name> : _ = _ ... in * : skip annotation and def, focus on body */
    | [MatchName(name), MatchDelimiter(":"), MatchSlot, MatchDelimiter("="), MatchSlot, MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
      switch (Pat.term_of(spine.pat)) {
      | Asc(inner_pat, _ty) when Option.equal(String.equal, Pat.is_var(inner_pat), Some(name)) =>
        [{
          focused: FocusExp(spine.body),
          focused_id: Exp.rep_id(spine.body),
          breadcrumb: "let " ++ name ++ " : _ = _ ... in ...",
        }]
      | _ => []
      }

    /* let <name> : match name, return whole or continue */
    | [MatchName(name)]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        {
          focused: FocusExp(spine.whole),
          focused_id: Exp.rep_id(spine.whole),
          breadcrumb: "let " ++ name,
        },
      ]
    | [MatchName(name), ...rest]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.whole)

    /* let _ = * : slot pattern, focus on def */
    | [MatchSlot, MatchDelimiter("="), MatchFocus] => [
        {
          focused: FocusExp(spine.def),
          focused_id: Exp.rep_id(spine.def),
          breadcrumb: "let _ = ...",
        },
      ]

    /* let _ ... in * : slot pattern, focus on body */
    | [MatchSlot, MatchEllipsis, MatchKeyword("in"), MatchFocus] => [
        {
          focused: FocusExp(spine.body),
          focused_id: Exp.rep_id(spine.body),
          breadcrumb: "let _ ... in ...",
        },
      ]

    | _ => []
    }

  /* Walk an if spine after "if" keyword */
  and walk_if_spine = (spine: if_spine, steps: sem_selector) =>
    switch (steps) {
    /* if * : focus on condition */
    | [MatchFocus] => [
        {
          focused: FocusExp(spine.cond),
          focused_id: Exp.rep_id(spine.cond),
          breadcrumb: "if ...",
        },
      ]

    /* if _ then * : focus on then branch */
    | [MatchSlot, MatchKeyword("then"), MatchFocus] => [
        {
          focused: FocusExp(spine.then_),
          focused_id: Exp.rep_id(spine.then_),
          breadcrumb: "if _ then ...",
        },
      ]

    /* if ... else * : focus on else branch */
    | [MatchEllipsis, MatchKeyword("else"), MatchFocus] => [
        {
          focused: FocusExp(spine.else_),
          focused_id: Exp.rep_id(spine.else_),
          breadcrumb: "if ... else ...",
        },
      ]

    /* if _ then _ else * : focus on else branch */
    | [
        MatchSlot,
        MatchKeyword("then"),
        MatchSlot,
        MatchKeyword("else"),
        MatchFocus,
      ] => [
        {
          focused: FocusExp(spine.else_),
          focused_id: Exp.rep_id(spine.else_),
          breadcrumb: "if _ then _ else ...",
        },
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
    /* case * : focus on scrutinee */
    | [MatchFocus] => [
        {
          focused: FocusExp(scrut),
          focused_id: Exp.rep_id(scrut),
          breadcrumb: "case ...",
        },
      ]

    /* case ... | <ctor> => * : find arm by constructor, focus on body */
    | [MatchEllipsis, MatchDelimiter("|"), ...rest] =>
      walk_pipe_in_rules(rules, rest)

    /* case _ | <ctor> => * : skip scrutinee, find arm */
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

  and walk_pipe_in_rules =
      (rules: list((Pat.t, Exp.t)), steps: sem_selector) =>
    switch (steps) {
    /* | <name> => * : find arm by constructor name */
    | [MatchName(name), MatchDelimiter("=>"), MatchFocus] =>
      rules
      |> List.filter_map(((pat, body)) =>
           switch (pat_name(pat)) {
           | Some(n) when String.equal(n, name) =>
             Some({
               focused: FocusExp(body),
               focused_id: Exp.rep_id(body),
               breadcrumb: "| " ++ name ++ " => ...",
             })
           | _ =>
             /* Also check for constructor patterns like Foo(x) */
             switch (Pat.term_of(pat)) {
             | Ap({term: Constructor(cname, _), _}, _)
                 when String.equal(cname, name) =>
               Some({
                 focused: FocusExp(body),
                 focused_id: Exp.rep_id(body),
                 breadcrumb: "| " ++ name ++ "(...) => ...",
               })
             | Constructor(cname, _) when String.equal(cname, name) =>
               Some({
                 focused: FocusExp(body),
                 focused_id: Exp.rep_id(body),
                 breadcrumb: "| " ++ name ++ " => ...",
               })
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

    /* | _ => * : wildcard, match any single arm body */
    | [MatchSlot, MatchDelimiter("=>"), MatchFocus] =>
      rules
      |> List.map(((pat, body)) => {
           let name =
             switch (pat_name(pat)) {
             | Some(n) => n
             | None => "_"
             };
           {
             focused: FocusExp(body),
             focused_id: Exp.rep_id(body),
             breadcrumb: "| " ++ name ++ " => ...",
           };
         })

    /* | _ => <more steps> : wildcard, continue into each arm body */
    | [MatchSlot, MatchDelimiter("=>"), ...rest] =>
      rules |> List.concat_map(((_pat, body)) => walk(rest, body))

    /* | _... <more> : skip zero or more arms, try rest at each position */
    | [MatchEllipsis, ...rest] =>
      let rec try_from = (remaining_rules) =>
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
      )

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
    let name_matches = (n) =>
      Option.equal(String.equal, name_opt, Some(n));
    switch (steps) {
    /* module M : bare name match. For module items, return FocusMod;
       for top-level module expressions, return FocusExp(whole). */
    | [MatchName(name)] when name_matches(name) =>
      switch (mod_item_opt) {
      | Some(item) => [
          {
            focused: FocusMod(item),
            focused_id: Mod.rep_id(item),
            breadcrumb: "module " ++ name,
          },
        ]
      | None => [
          {
            focused: FocusExp(whole),
            focused_id: Exp.rep_id(whole),
            breadcrumb: "module " ++ name,
          },
        ]
      }

    /* module M = * : focus on module def */
    | [MatchName(name), MatchDelimiter("="), MatchFocus]
        when name_matches(name) => [
        {
          focused: FocusExp(def),
          focused_id: Exp.rep_id(def),
          breadcrumb: "module " ++ name ++ " = ...",
        },
      ]

    /* module M = ... : match by name, continue into def */
    | [MatchName(name), MatchDelimiter("="), ...rest]
        when name_matches(name) =>
      walk(rest, def)

    /* module M _... in * : skip def, focus on body */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), MatchFocus]
        when name_matches(name) =>
      switch (body_opt) {
      | Some(body) => [
          {
            focused: FocusExp(body),
            focused_id: Exp.rep_id(body),
            breadcrumb: "module " ++ name ++ " ... in ...",
          },
        ]
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
      | Some(item) => [
          {
            focused: FocusMod(item),
            focused_id: Mod.rep_id(item),
            breadcrumb: "module " ++ name,
          },
        ]
      | None => [
          {
            focused: FocusExp(whole),
            focused_id: Exp.rep_id(whole),
            breadcrumb: "module " ++ name,
          },
        ]
      }
    | [MatchName(name), ...rest] when name_matches(name) =>
      switch (body_opt) {
      | Some(body) => walk(rest, body)
      | None => walk(rest, def)
      }

    /* module _ = * : wildcard name, focus on def */
    | [MatchSlot, MatchDelimiter("="), MatchFocus] => [
        {
          focused: FocusExp(def),
          focused_id: Exp.rep_id(def),
          breadcrumb: "module " ++ name_str ++ " = ...",
        },
      ]

    /* module _ = <more> : wildcard name, continue into def */
    | [MatchSlot, MatchDelimiter("="), ...rest] =>
      walk(rest, def)

    /* module _... in * : skip name and def, focus on body */
    | [MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
      switch (body_opt) {
      | Some(body) => [
          {
            focused: FocusExp(body),
            focused_id: Exp.rep_id(body),
            breadcrumb: "module " ++ name_str ++ " ... in ...",
          },
        ]
      | None => []
      }

    /* module _... in <more> : skip to body, continue */
    | [MatchEllipsis, MatchKeyword("in"), ...rest] =>
      switch (body_opt) {
      | Some(body) => walk(rest, body)
      | None => []
      }

    /* module * : focus on whole module expression */
    | [MatchFocus] => [
        {
          focused: FocusExp(whole),
          focused_id: Exp.rep_id(whole),
          breadcrumb: "module " ++ name_str,
        },
      ]

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
    let name_matches = (n) =>
      Option.equal(String.equal, name_opt, Some(n));
    let name_str = Option.value(~default="_", name_opt);
    /* Helper: handle steps after the name has been matched/consumed */
    let walk_after_name = (remaining: sem_selector) =>
      switch (remaining) {
      /* type T = * : focus on type definition */
      | [MatchDelimiter("="), MatchFocus] =>
        switch (tdef_opt) {
        | Some(tdef) => [
            {
              focused: FocusTyp(tdef),
              focused_id: Typ.rep_id(tdef),
              breadcrumb: "type " ++ name_str ++ " = ...",
            },
          ]
        | None => []
        }
      /* type T _... in * : skip def, focus on body */
      | [MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
        switch (body_opt) {
        | Some(body) => [
            {
              focused: FocusExp(body),
              focused_id: Exp.rep_id(body),
              breadcrumb: "type " ++ name_str ++ " ... in ...",
            },
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
      | Some(item) => [
          {
            focused: FocusMod(item),
            focused_id: Mod.rep_id(item),
            breadcrumb: "type " ++ name_str,
          },
        ]
      | None => [
          {
            focused: FocusExp(whole),
            focused_id: Exp.rep_id(whole),
            breadcrumb: "type " ++ name_str,
          },
        ]
      };
    switch (steps) {
    /* type T : bare name match */
    | [MatchName(name)] when name_matches(name) =>
      focus_whole_or_mod()
    /* type T * : bare name + implicit star */
    | [MatchName(name), MatchFocus] when name_matches(name) =>
      focus_whole_or_mod()

    /* type T <more> : match by name, continue */
    | [MatchName(name), ...rest] when name_matches(name) =>
      walk_after_name(rest)

    /* type _ <more> : wildcard name */
    | [MatchSlot, ...rest] =>
      walk_after_name(rest)

    /* type _... in * : skip name and def */
    | [MatchEllipsis, MatchKeyword("in"), MatchFocus] =>
      switch (body_opt) {
      | Some(body) => [
          {
            focused: FocusExp(body),
            focused_id: Exp.rep_id(body),
            breadcrumb: "type " ++ name_str ++ " ... in ...",
          },
        ]
      | None => []
      }

    | [MatchEllipsis, MatchKeyword("in"), ...rest] =>
      switch (body_opt) {
      | Some(body) => walk(rest, body)
      | None => []
      }

    /* type * : focus on whole type alias expression */
    | [MatchFocus] => [
        {
          focused: FocusExp(whole),
          focused_id: Exp.rep_id(whole),
          breadcrumb: "type " ++ name_str,
        },
      ]

    | _ => []
    };
  }

  /* Walk a fun spine after "fun" keyword */
  and walk_fun_spine = (spine: fun_spine, steps: sem_selector) =>
    switch (steps) {
    /* fun * : focus on the pattern (as an expression context — actually
       we can't return a Pat as Exp, so focus on whole fun) */
    | [MatchFocus] => [
        {
          focused: FocusExp(spine.body),
          focused_id: Exp.rep_id(spine.body),
          breadcrumb: "fun ... -> ...",
        },
      ]

    /* fun _ -> * : skip pattern, focus on body */
    | [MatchSlot, MatchDelimiter("->"), MatchFocus] => [
        {
          focused: FocusExp(spine.body),
          focused_id: Exp.rep_id(spine.body),
          breadcrumb: "fun _ -> ...",
        },
      ]

    /* fun _ -> <more> : skip pattern, continue in body */
    | [MatchSlot, MatchDelimiter("->"), ...rest] =>
      walk(rest, spine.body)

    /* fun ... -> * : skip pattern via ellipsis, focus on body */
    | [MatchEllipsis, MatchDelimiter("->"), MatchFocus] => [
        {
          focused: FocusExp(spine.body),
          focused_id: Exp.rep_id(spine.body),
          breadcrumb: "fun ... -> ...",
        },
      ]

    /* fun ... -> <more> : skip pattern via ellipsis, continue in body */
    | [MatchEllipsis, MatchDelimiter("->"), ...rest] =>
      walk(rest, spine.body)

    /* fun <name> -> * : match pattern by name, focus on body */
    | [MatchName(name), MatchDelimiter("->"), MatchFocus]
        when Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        {
          focused: FocusExp(spine.body),
          focused_id: Exp.rep_id(spine.body),
          breadcrumb: "fun " ++ name ++ " -> ...",
        },
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
    /* test * : focus on the test body */
    | [MatchFocus] => [
        {
          focused: FocusExp(spine.body),
          focused_id: Exp.rep_id(spine.body),
          breadcrumb: "test ...",
        },
      ]

    /* test _ end [*] : match slot, then end keyword → whole test
       The MatchFocus variant handles the implicit star rule. */
    | [MatchSlot, MatchKeyword("end")]
    | [MatchEllipsis, MatchKeyword("end")]
    | [MatchSlot, MatchKeyword("end"), MatchFocus]
    | [MatchEllipsis, MatchKeyword("end"), MatchFocus] => [
        {
          focused: FocusExp(spine.whole),
          focused_id: Exp.rep_id(spine.whole),
          breadcrumb: "test _ end",
        },
      ]

    /* test _ * : match slot, then focus on body */
    | [MatchSlot, MatchFocus]
    | [MatchEllipsis, MatchFocus] => [
        {
          focused: FocusExp(spine.body),
          focused_id: Exp.rep_id(spine.body),
          breadcrumb: "test ...",
        },
      ]

    /* test _ <more> : match slot, continue into body */
    | [MatchSlot, ...rest]
    | [MatchEllipsis, ...rest] => walk(rest, spine.body)

    /* test <more> : continue matching inside the body */
    | rest => walk(rest, spine.body)
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
        | [item, ..._] => [
            {
              focused: FocusExp(item),
              focused_id: Exp.rep_id(item),
              breadcrumb: "",
            },
          ]
        | [] => []
        }

      /* Ellipsis + Focus: skip to last, focus on it */
      | [MatchEllipsis, MatchFocus, ..._] =>
        switch (List.rev(items)) {
        | [last, ..._] => [
            {
              focused: FocusExp(last),
              focused_id: Exp.rep_id(last),
              breadcrumb: "",
            },
          ]
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
      /* Surface ModLet items as let_spines so `let x = *` finds them.
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
  | MatchFocus => "*"
  | MatchSlot => "_"
  | MatchEllipsis => "_..."
  | MatchName(s) => s
  | MatchNameIndex(s, idx) => s ++ "#" ++ string_of_int(idx)
  | MatchKeyword(kw) => kw
  | MatchDelimiter(d) => d
  | EnterBinderDef(s) => s ++ "/"
  | DescendInto => "\\..."
  };

/* Format a prefix of sem_selector steps as a readable string */
let steps_to_string = (steps: sem_selector): string =>
  steps |> List.map(step_to_string) |> String.concat(" ");

/* Diagnose why a selector produced no matches.
   Returns a helpful error message with:
   - How far the selector matched before failing
   - Available names at the failure point
   - "Did you mean?" suggestions for close name mismatches */
let diagnose_no_match =
    (selector_str: string, root: Exp.t): string => {
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
        List.filteri((i, _) => i < matched_depth, steps)
        |> steps_to_string;
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
