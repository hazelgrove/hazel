open Language;

/* Selector language for addressing Hazel syntax subtrees.
   See plans/Hazel-Agent-Path-Selector-Language.md for the full spec.

   Core operators:
   - `_`    : matches one syntactic slot
   - `_...` : matches zero or more slots along current spine
   - `⋱`    : descendant search (match P, then find Q inside)
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
  | Descend /* ⋱ or \_ */
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
  | Chain(list(string)) /* A/B/C - binder chain */
  | Name(string); /* bare name */

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
  | DescendInto /* descend into matched subtree */
  | EnterBinderDef(string); /* find binder by name, enter its def */

type sem_selector = list(sem_step);

/* === Match result === */

type match_result = {
  focused: Exp.t,
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
      | "_..." | "..." | "\xe2\x80\xa6" => Ellipsis /* … UTF-8 */
      | "*" => Focus
      | "\\_" | "\xe2\x8b\xb1" => Descend /* ⋱ UTF-8 */
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
      | s when String.contains(s, '/') =>
        let segments =
          s
          |> String.split_on_char('/')
          |> List.filter(seg => String.length(seg) > 0);
        switch (segments) {
        | [single] => Name(single)
        | segs => Chain(segs)
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
    | [Chain(segments), ...rest] =>
      /* A/B/C expands to: EnterBinderDef(A), EnterBinderDef(B), MatchName(C) */
      let chain_steps =
        switch (List.rev(segments)) {
        | [] => []
        | [last, ...rev_init] =>
          let init = List.rev(rev_init);
          List.map(s => EnterBinderDef(s), init) @ [MatchName(last)];
        };
      chain_steps @ go(rest);
    | [Name(s), ...rest] => [MatchName(s), ...go(rest)]
    };
  go(sel);
};

/* === Resolution === */

/* Helper: get the pattern name from a Pat.t */
let pat_name = (p: Pat.t): option(string) =>
  switch (Pat.term_of(p)) {
  | Var(name) => Some(name)
  | _ => None
  };

/* Helper: get the tpat name from a TPat.t */
let tpat_name = (tp: TPat.t): option(string) =>
  switch (tp.term) {
  | Var(name) => Some(name)
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
  | Module(items) =>
    /* Search module items for a let/type named `name` */
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
          | _ => None
          }
        },
      None,
      items,
    )
  | _ => None
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

/* Resolve a selector against an expression, returning all matches */
let resolve = (sel: selector, root: Exp.t): list(match_result) => {
  /* Walk the selector steps against a current expression context.
     Returns list of (focused_exp) for each match. */
  let rec walk =
          (steps: sem_selector, current: Exp.t): list(match_result) =>
    switch (steps) {
    | [] => []

    /* Focus: return the current expression as match */
    | [MatchFocus] => [
        {
          focused: current,
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
            focused: let_exp,
            focused_id: Exp.rep_id(let_exp),
            breadcrumb: name,
          },
        ]
      | None =>
        /* Also check module items */
        switch (find_binder_in_exp(name, current)) {
        | Some((def, _)) => [
            {
              focused: def,
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
            focused: def,
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
            focused: body,
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

    /* let keyword: try all lets in the chain */
    | [MatchKeyword("let"), ...rest] =>
      find_all_lets(current)
      |> List.concat_map(spine => walk_let_spine(spine, rest))

    /* if keyword: expect current to be an If */
    | [MatchKeyword("if"), ...rest] =>
      switch (Exp.term_of(current)) {
      | If(cond, then_, else_) =>
        walk_if_spine({cond, then_, else_, whole: current}, rest)
      | _ => []
      }

    /* case keyword: expect current to be a Match */
    | [MatchKeyword("case"), ...rest] =>
      switch (Exp.term_of(current)) {
      | Match(scrut, rules) =>
        walk_case_spine(current, scrut, rules, rest)
      | _ => []
      }

    /* module keyword */
    | [MatchKeyword("module"), ...rest] =>
      switch (Exp.term_of(current)) {
      | Let(pat, def, _body) =>
        switch (Exp.term_of(def)) {
        | Module(_) =>
          switch (pat_name(pat)) {
          | Some(_) => walk_after_module_kw(pat, def, current, rest)
          | None => []
          }
        | _ => []
        }
      | _ => []
      }

    /* type keyword */
    | [MatchKeyword("type"), ...rest] =>
      switch (Exp.term_of(current)) {
      | TyAlias(tpat, _tdef, _body) =>
        walk_after_type_kw(tpat, current, rest)
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
          focused: spine.whole,
          focused_id: Exp.rep_id(spine.whole),
          breadcrumb: "",
        },
      ]
    | [MatchFocus] => [
        {
          focused: spine.whole,
          focused_id: Exp.rep_id(spine.whole),
          breadcrumb: "",
        },
      ]

    /* let <name> = * : focus on definition */
    | [MatchName(name), MatchDelimiter("="), MatchFocus]
        when
          Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        {
          focused: spine.def,
          focused_id: Exp.rep_id(spine.def),
          breadcrumb: "let " ++ name ++ " = ...",
        },
      ]

    /* let <name> = * ... : focus on definition, continue */
    | [MatchName(name), MatchDelimiter("="), MatchFocus, ...rest]
        when
          Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.def)

    /* let <name> ... in * : focus on body */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), MatchFocus]
        when
          Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        {
          focused: spine.body,
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
        when
          Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.body)

    /* let <name> = ... : match but continue through remaining steps */
    | [MatchName(name), MatchDelimiter("="), ...rest]
        when
          Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.def)

    /* let <name> ... in <more> : skip to body */
    | [MatchName(name), MatchEllipsis, MatchKeyword("in"), ...rest]
        when
          Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.body)

    /* let <name> : match name, return whole or continue */
    | [MatchName(name)]
        when
          Option.equal(String.equal, pat_name(spine.pat), Some(name)) => [
        {
          focused: spine.whole,
          focused_id: Exp.rep_id(spine.whole),
          breadcrumb: "let " ++ name,
        },
      ]
    | [MatchName(name), ...rest]
        when
          Option.equal(String.equal, pat_name(spine.pat), Some(name)) =>
      walk(rest, spine.whole)

    /* let _ = * : slot pattern, focus on def */
    | [MatchSlot, MatchDelimiter("="), MatchFocus] => [
        {
          focused: spine.def,
          focused_id: Exp.rep_id(spine.def),
          breadcrumb: "let _ = ...",
        },
      ]

    /* let _ ... in * : slot pattern, focus on body */
    | [MatchSlot, MatchEllipsis, MatchKeyword("in"), MatchFocus] => [
        {
          focused: spine.body,
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
          focused: spine.cond,
          focused_id: Exp.rep_id(spine.cond),
          breadcrumb: "if ...",
        },
      ]

    /* if _ then * : focus on then branch */
    | [MatchSlot, MatchKeyword("then"), MatchFocus] => [
        {
          focused: spine.then_,
          focused_id: Exp.rep_id(spine.then_),
          breadcrumb: "if _ then ...",
        },
      ]

    /* if ... else * : focus on else branch */
    | [MatchEllipsis, MatchKeyword("else"), MatchFocus] => [
        {
          focused: spine.else_,
          focused_id: Exp.rep_id(spine.else_),
          breadcrumb: "if ... else ...",
        },
      ]

    /* if _ then _ else * : focus on else branch */
    | [MatchSlot, MatchKeyword("then"), MatchSlot, MatchKeyword("else"), MatchFocus] => [
        {
          focused: spine.else_,
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
          focused: scrut,
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
               focused: body,
               focused_id: Exp.rep_id(body),
               breadcrumb: "| " ++ name ++ " => ...",
             })
           | _ =>
             /* Also check for constructor patterns like Foo(x) */
             switch (Pat.term_of(pat)) {
             | Ap({term: Constructor(cname, _), _}, _)
                 when String.equal(cname, name) =>
               Some({
                 focused: body,
                 focused_id: Exp.rep_id(body),
                 breadcrumb: "| " ++ name ++ "(...) => ...",
               })
             | Constructor(cname, _) when String.equal(cname, name) =>
               Some({
                 focused: body,
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
               | Constructor(cname, _) when String.equal(cname, name) =>
                 true
               | _ => false
               }
             };
           if (matches) {
             walk(rest, body);
           } else {
             [];
           };
         })

    | _ => []
    }

  /* Walk after "module" keyword */
  and walk_after_module_kw =
      (pat: Pat.t, def: Exp.t, whole: Exp.t, steps: sem_selector) =>
    switch (steps) {
    /* module M = * : focus on module def */
    | [MatchName(name), MatchDelimiter("="), MatchFocus]
        when Option.equal(String.equal, pat_name(pat), Some(name)) => [
        {
          focused: def,
          focused_id: Exp.rep_id(def),
          breadcrumb: "module " ++ name ++ " = ...",
        },
      ]

    /* module M = ... : match, continue into def */
    | [MatchName(name), MatchDelimiter("="), ...rest]
        when Option.equal(String.equal, pat_name(pat), Some(name)) =>
      walk(rest, def)

    | _ =>
      let _ = whole;
      [];
    }

  /* Walk after "type" keyword */
  and walk_after_type_kw =
      (tpat: TPat.t, whole: Exp.t, steps: sem_selector) =>
    switch (steps) {
    | [MatchName(name), ...rest]
        when Option.equal(String.equal, tpat_name(tpat), Some(name)) =>
      walk(rest, whole)
    | _ => []
    }

  /* Find the Let node for a given binder name */
  and find_let_node = (name: string, e: Exp.t): option(Exp.t) =>
    switch (Exp.term_of(e)) {
    | Let(pat, _def, body) =>
      switch (pat_name(pat)) {
      | Some(n) when String.equal(n, name) => Some(e)
      | _ => find_let_node(name, body)
      }
    | _ => None
    }

  /* Find all Let nodes in an expression chain */
  and find_all_lets = (e: Exp.t): list(let_spine) =>
    switch (Exp.term_of(e)) {
    | Let(pat, def, body) => [
        {pat, def, body, whole: e},
        ...find_all_lets(body),
      ]
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
      | Tuple(es) =>
        List.concat_map(e => descend_all(e, remaining), es)
      | ListLit(es) =>
        List.concat_map(e => descend_all(e, remaining), es)
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
            | _ => []
            },
          items,
        )
      | Asc(e, _) => descend_all(e, remaining)
      | _ => []
      };
    here @ children;
  };

  walk(elaborate(sel), root);
};

/* === Convenience: parse + resolve === */

let query = (selector_str: string, root: Exp.t): list(match_result) => {
  let sel = parse(selector_str);
  resolve(sel, root);
};

/* For edit actions: require exactly one match */
let query_unique =
    (selector_str: string, root: Exp.t)
    : result(match_result, string) => {
  switch (query(selector_str, root)) {
  | [] => Error("No match for selector: " ++ selector_str)
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

/* Print a match result's focused expression as text */
let print_match = (m: match_result): string => {
  let segment =
    ExpToSegment.exp_to_segment(
      ~settings=ExpToSegment.Settings.of_core(~inline=true, CoreSettings.on),
      m.focused,
    );
  Printer.of_segment(~holes="?", segment);
};
