/* Abbreviate a term for display, specifically for the live
 * value probe projector. This is currently specialized for
 * expressions which are (at least partially) values.
 *
 * This is an ugly rough approach, and should be rewritten when
 * we have projectors (in particular, fold) within value displays.
 *
 * This approach ends up duplicating way too much info
 * with ExpToSeg. This should probably be rewritten to
 * use that somehow. */

open Util;

let flat_ellipses = "…"; //"⋱"; // "┄"
let flat_ellipses_term = () =>
  IdTagged.fresh(Invalid(flat_ellipses): Exp.term);
let flat_ellipses_term_pat = (): TermBase.pat_t =>
  IdTagged.fresh(Invalid(flat_ellipses): Pat.term);
let is_flat_ellipses = (term: IdTagged.t(Exp.term)): bool =>
  switch (term.term) {
  | Invalid(s) => s == flat_ellipses
  | Atom(String(s)) => s == flat_ellipses
  | Constructor(s, _) => s == flat_ellipses
  | Var(s) => s == flat_ellipses
  | _ => false
  };
let available = ref(0);

module AbbrevBudget = {
  let label_estimated_length = (~label: Exp.t): int =>
    switch (label |> Exp.term_of) {
    | Label(name) => String.length(name)
    | Var(name) => String.length(name)
    | Invalid(str) => String.length(str)
    | Atom(String(str)) => String.length(str)
    | _ => 0
    };

  let with_budget = (~budget: int, ~run: unit => 'a): ('a, int) => {
    let prev: int = available^;
    let limited: int =
      if (budget < 0) {
        0;
      } else if (budget > prev) {
        prev;
      } else {
        budget;
      };
    available := limited;
    let result: 'a = run();
    let remaining: int = available^;
    let consumed: int = limited - remaining;
    let next_total: int = {
      let diff: int = prev - consumed;
      diff < 0 ? 0 : diff;
    };
    available := next_total;
    (result, consumed);
  };

  let split_evenly = (~total: int, ~parts: int): list(int) =>
    if (parts <= 0) {
      [];
    } else if (total <= 0) {
      let rec zeros = (count: int, acc: list(int)): list(int) =>
        count <= 0 ? acc : zeros(count - 1, [0, ...acc]);
      zeros(parts, []);
    } else {
      let base: int = total / parts;
      let remainder: int = total mod parts;
      let rec distribute = (index: int, acc: list(int)): list(int) =>
        index < 0
          ? acc
          : distribute(
              index - 1,
              [base + (index < remainder ? 1 : 0), ...acc],
            );
      distribute(parts - 1, []);
    };

  let label_cap = (~label: Exp.t, ~available: int): int => {
    let estimated_label_len: int = label_estimated_length(~label);
    let desired: int = estimated_label_len + 1;
    let half: int = available / 2;
    let proposed: int = half < desired ? desired : half;
    let capped: int = proposed < 1 ? 1 : proposed;
    capped > available ? available : capped;
  };

  let label_min_budget = (~label: Exp.t): int => {
    let est: int = label_estimated_length(~label);
    if (est >= 3) {
      3;
    } else if (est == 2) {
      2;
    } else {
      1;
    };
  };
};

module AbbrevSequence = {
  let separator_cost: int = 2;
  let ellipsis_cost: int = 3;

  let rec consume =
          (
            items: list(Exp.t),
            budgets: list(int),
            ~abbreviate: Exp.t => Exp.t,
          )
          : list(Exp.t) =>
    switch (items, budgets) {
    | ([], _) => []
    | ([item, ...rest], [budget, ...rest_budgets]) =>
      let (abbreviated_item: Exp.t, _): (Exp.t, int) =
        AbbrevBudget.with_budget(~budget, ~run=() => abbreviate(item));
      if (rest == []) {
        [abbreviated_item];
      } else if (available^ > ellipsis_cost) {
        available := available^ - separator_cost;
        [abbreviated_item, ...consume(rest, rest_budgets, ~abbreviate)];
      } else {
        available := available^ - ellipsis_cost;
        [abbreviated_item, flat_ellipses_term()];
      };
    | (_, _) => []
    };

  let run = (~items: list(Exp.t), ~abbreviate: Exp.t => Exp.t): list(Exp.t) => {
    let count: int = List.length(items);
    if (count <= 0) {
      [];
    } else {
      let separators_needed: int =
        count <= 1 ? 0 : (count - 1) * separator_cost;
      let available_now: int = available^;
      let available_for_items: int = {
        let diff: int = available_now - separators_needed;
        diff < 0 ? 0 : diff;
      };
      let budgets: list(int) =
        AbbrevBudget.split_evenly(~total=available_for_items, ~parts=count);
      consume(items, budgets, ~abbreviate);
    };
  };
};

let abbreviate_str = (min_len: int, s: string): string => {
  let len = String.length(s);
  let ellipsis = flat_ellipses;
  if (len < 2) {
    s;
  } else if (len <= min_len || min_len < 1) {
    available := available^ - len;
    s;
  } else if (min_len < 1) {
    let str = String.sub(s, 0, 1) ++ ellipsis;
    available := available^ - String.length(str);
    str;
  } else {
    let str = String.sub(s, 0, min_len - 1) ++ ellipsis;
    available := available^ - String.length(str);
    str;
  };
};

let is_comment_token = (s: string): bool => {
  let len: int = String.length(s);
  if (len == 1) {
    s == "#";
  } else if (len >= 2 && s.[0] == '#' && s.[len - 1] == '#') {
    let rec loop = (idx: int): bool =>
      if (idx >= len - 1) {
        true;
      } else {
        let ch: char = s.[idx];
        ch == '#' || ch == '\n' ? false : loop(idx + 1);
      };
    loop(1);
  } else {
    false;
  };
};

let take_grapheme_prefix =
    (count: int, clusters: list(string)): list(string) => {
  let rec loop =
          (remaining: int, rest: list(string), acc: list(string))
          : list(string) =>
    if (remaining <= 0) {
      List.rev(acc);
    } else {
      switch (rest) {
      | [] => List.rev(acc)
      | [hd, ...tl] => loop(remaining - 1, tl, [hd, ...acc])
      };
    };
  loop(count, clusters, []);
};

let abbreviate_string_token = (~min_len: int, s: string): string => {
  let grapheme_len: int = Unicode.length(s);
  let ellipsis: string = flat_ellipses;
  if (grapheme_len < 2) {
    s;
  } else if (grapheme_len <= min_len || min_len < 1) {
    available := available^ - String.length(s);
    s;
  } else {
    let clusters: list(string) = Unicode.to_list(s);
    let prefix_clusters: list(string) =
      take_grapheme_prefix(min_len > 0 ? min_len - 1 : 0, clusters);
    let prefix: string = String.concat("", prefix_clusters);
    let truncated: string = prefix ++ ellipsis;
    available := available^ - String.length(truncated);
    truncated;
  };
};

let indet_term: Exp.term = Invalid("?");
let indet_term_typ: Typ.term = Unknown(Internal);
let indet_term_pat: Pat.term = Invalid("?");
let indet_term_rul: Rul.term = Invalid("?");
let indet_term_tpat: TPat.term = Invalid("?");

let rec abbreviate_exp = (exp: Exp.t): Exp.t => {
  let rewrap = (term: Exp.term): Exp.t => {
    {
      ...exp,
      term,
    };
  };
  let wrap_or = (term, str): Exp.term =>
    if (available^ > String.length(str)) {
      available := available^ - String.length(str);
      term;
    } else {
      Invalid(abbreviate_str(available^, str));
    };

  let abbreviate_list_seq = (xs: list(Exp.t)): list(Exp.t) => {
    let rec go = (seq: list(Exp.t)): list(Exp.t) =>
      switch (seq) {
      | [] => []
      | [x, ...rest] =>
        let head: Exp.t = abbreviate_exp(x);
        if (available^ > 3) {
          available := available^ - 2; // comma space
          [head, ...go(rest)];
        } else if (rest == []) {
          [head];
        } else {
          available := available^ - 3;
          [head, flat_ellipses_term()];
        };
      };
    go(xs);
  };
  let abbreviate_tuple_seq = (xs: list(Exp.t)): list(Exp.t) =>
    AbbrevSequence.run(~items=xs, ~abbreviate=abbreviate_exp);

  // Helper to handle cases where we need to check available space and potentially return indet_term
  let handle_op_indet =
      (
        ~cost: int,
        ~make_term: (Exp.t, Exp.t) => Exp.term,
        e1: Exp.t,
        e2: Exp.t,
      )
      : Exp.term =>
    if (available^ <= cost) {
      indet_term;
    } else {
      available := available^ - cost;
      let e1' = abbreviate_exp(e1);
      if (available^ > 0) {
        let e2' = abbreviate_exp(e2);
        make_term(e1', e2');
      } else {
        e1'.term;
      };
    };

  // Helper for unary operations
  let handle_unary =
      (~cost: int, ~make_term: Exp.t => Exp.term, e: Exp.t): Exp.term =>
    if (available^ <= cost) {
      indet_term;
    } else {
      available := available^ - cost;
      make_term(abbreviate_exp(e));
    };

  let term: Exp.term =
    switch (exp |> Exp.term_of) {
    | Fun(_p, _e, _, Some(s)) => Invalid("<" ++ s ++ ">")
    | Fun(_p, _e, _, None) => Invalid("<>")
    | BuiltinFun(_f) => Invalid("<>")
    | Tuple([e]) => Tuple([abbreviate_exp(e)])
    | DynamicErrorHole(_exp, err) =>
      Invalid("<" ++ InvalidOperationError.show(err) ++ ">")

    // Atomic string cases
    | Invalid(x) =>
      let abbreviated =
        is_comment_token(x)
          ? abbreviate_string_token(~min_len=available^, x)
          : abbreviate_str(available^, x);
      Invalid(abbreviated);
    | Atom(String(s)) =>
      let str = abbreviate_string_token(~min_len=available^, s);
      available := available^ - 2; // for quotes in printed representation
      Atom(String(str));
    | Var(v) => Var(abbreviate_str(available^, v))
    | Label(v) => Label(abbreviate_str(available^, v))
    | ExplicitNonlabel => ExplicitNonlabel
    | Constructor(c, t) => Constructor(abbreviate_str(available^, c), t)
    | LivelitName(v) => LivelitName(abbreviate_str(available^, v))

    // Other atomic cases
    | EmptyHole => EmptyHole
    | ListLit([]) => ListLit([])
    | Tuple([]) => Tuple([])
    | Deferral(pos) => Deferral(pos)
    | Undefined => wrap_or(Undefined, "undefined")
    | Atom(Bool(b)) => wrap_or(Atom(Bool(b)), string_of_bool(b))
    | Atom(Int(n) | Nat(n)) =>
      //TODO: smarter number summarization?
      wrap_or(Atom(Int(n)), Bigint.to_string(n))
    | Atom(SInt(n)) => wrap_or(Atom(SInt(n)), string_of_int(n))
    | Atom(Float(f)) =>
      Invalid(abbreviate_str(available^, string_of_float(f)))

    // composite literal cases
    | ListLit(xs) =>
      if (available^ <= 3) {
        // minimum case: […]
        available := available^ - 3;
        ListLit([flat_ellipses_term()]);
      } else {
        available := available^ - 2; // square brackets
        ListLit(abbreviate_list_seq(xs));
      }
    | Tuple([_, _, ..._] as xs) => Tuple(abbreviate_tuple_seq(xs))
    | TupLabel(e1, e2) =>
      if (available^ <= 3) {
        Invalid(flat_ellipses);
      } else {
        available := available^ - 3;
        let remaining: int = available^;
        let label_min_budget: int =
          if (remaining <= 0) {
            0;
          } else {
            min(AbbrevBudget.label_min_budget(~label=e1), remaining);
          };
        let label_preferred: int =
          min(
            AbbrevBudget.label_cap(~label=e1, ~available=remaining),
            remaining,
          );
        let label_budget: int =
          if (label_preferred < label_min_budget) {
            label_min_budget;
          } else {
            label_preferred;
          };
        let (label', _): (Exp.t, int) =
          AbbrevBudget.with_budget(~budget=label_budget, ~run=() =>
            abbreviate_exp(e1)
          );
        let (value', _): (Exp.t, int) =
          AbbrevBudget.with_budget(~budget=available^, ~run=() =>
            abbreviate_exp(e2)
          );
        TupLabel(label', value');
      }
    | Dot(e1, e2) =>
      if (available^ <= 3) {
        Invalid(flat_ellipses);
      } else {
        available := available^ - 3;
        Dot(abbreviate_exp(e1), abbreviate_exp(e2));
      }
    | Ap(Forward, {term: Constructor(_str, _), _} as konst, arg) =>
      let konst = abbreviate_exp(konst);
      available := available^ - 2;
      let arg =
        if (available^ > 0) {
          abbreviate_exp(arg);
        } else {
          flat_ellipses_term();
        };
      Ap(Forward, konst, arg);
    | Parens(e)
    | Probe(e, _) =>
      available := available^ - 2;
      Parens(abbreviate_exp(e));

    // Ascriptions

    | Asc(e, t1) =>
      handle_op_indet(
        ~cost=3, // " : "
        ~make_term=(e', _) => Asc(e', t1),
        e,
        e // dummy second arg since Asc only has one expression
      )
    // Indeterminant forms

    // List operations
    | Cons(e1, e2) =>
      handle_op_indet(
        ~cost=4, // " :: "
        ~make_term=(e1', e2') => Cons(e1', e2'),
        e1,
        e2,
      )
    | ListConcat(e1, e2) =>
      handle_op_indet(
        ~cost=3, // " @ "
        ~make_term=(e1', e2') => ListConcat(e1', e2'),
        e1,
        e2,
      )

    // Unary operations
    | UnOp(Bool(Not), e) =>
      handle_unary(
        ~cost=1, // "!"
        ~make_term=e' => UnOp(Bool(Not), e'),
        e,
      )
    | UnOp(SInt(Minus), e) =>
      handle_unary(
        ~cost=1, // "-"
        ~make_term=e' => UnOp(SInt(Minus), e'),
        e,
      )
    | UnOp(Float(Minus), e) =>
      handle_unary(
        ~cost=1, // "~"
        ~make_term=e' => UnOp(Float(Minus), e'),
        e,
      )
    | UnOp(Nat(Minus), e) =>
      handle_unary(
        ~cost=1, // "-"
        ~make_term=e' => UnOp(Nat(Minus), e'),
        e,
      )
    | UnOp(Int(Minus), e) =>
      handle_unary(
        ~cost=1, // "-"
        ~make_term=e' => UnOp(Int(Minus), e'),
        e,
      )
    | UnOp(Meta(Unquote), e) =>
      handle_unary(
        ~cost=1, // "$"
        ~make_term=e' => UnOp(Meta(Unquote), e'),
        e,
      )

    // Binary operations
    | BinOp(op, e1, e2) =>
      let op_str = Operators.bin_op_to_string(op);

      if (available^ <= String.length(op_str)) {
        indet_term;
      } else {
        available := available^ - String.length(op_str);
        let e1' = abbreviate_exp(e1);
        if (available^ > 0) {
          let e2' = abbreviate_exp(e2);
          BinOp(op, e1', e2');
        } else {
          e1'.term;
        };
      };

    | TupleExtension(e1, e2) =>
      if (available^ <= 3) {
        indet_term;
      } else {
        available := available^ - 3; // "..."
        let e1' = abbreviate_exp(e1);
        if (available^ > 0) {
          let e2' = abbreviate_exp(e2);
          TupleExtension(e1', e2');
        } else {
          e1'.term;
        };
      }
    | Ap(Forward, e1, e2) =>
      if (available^ <= 1) {
        indet_term;
      } else {
        available := available^ - 1; // space between terms
        let e1' = abbreviate_exp(e1);
        if (available^ > 0) {
          let e2' = abbreviate_exp(e2);
          Ap(Forward, e1', e2');
        } else {
          e1'.term;
        };
      }

    //similar to ap, except with builtin tuple for args
    | DeferredAp(e, es) =>
      if (available^ <= 1) {
        indet_term;
      } else {
        available := available^ - 1; // space between terms
        let e' = abbreviate_exp(e);
        if (available^ > 0) {
          let es' = List.map((e: Exp.t) => abbreviate_exp(e), es);
          DeferredAp(e', es');
        } else {
          e'.term;
        };
      }

    | Test(e) =>
      handle_unary(
        ~cost=9, // "test " + " end"
        ~make_term=e' => Test(e'),
        e,
      )
    | HintedTest(e, hint) =>
      handle_op_indet(
        ~cost=15, // "hint " + " test " + " end"
        ~make_term=(e', hint') => HintedTest(e', hint'),
        e,
        hint,
      )
    | Seq(e1, e2) =>
      handle_op_indet(
        ~cost=2, // "; "
        ~make_term=(e1', e2') => Seq(e1', e2'),
        e1,
        e2,
      )
    | If(e1, e2, e3) =>
      if (available^ <= 14) {
        // "if then else "
        indet_term;
      } else {
        available := available^ - 14;
        let e1' = abbreviate_exp(e1);
        if (available^ > 0) {
          let e2' = abbreviate_exp(e2);
          if (available^ > 0) {
            let e3' = abbreviate_exp(e3);
            If(e1', e2', e3');
          } else {
            e2'.term;
          };
        } else {
          e1'.term;
        };
      }
    | Ap(Reverse, e1, e2) =>
      handle_op_indet(
        ~cost=1, // space between terms
        ~make_term=(e1', e2') => Ap(Reverse, e1', e2'),
        e1,
        e2,
      )

    | Let(p, e1, e2) =>
      if (available^ < 3) {
        indet_term;
      } else if (available^ <= 3) {
        Invalid("let");
      } else if (available^ <= 4) {
        Invalid("let…");
      } else if (available^ <= 6) {
        Invalid("let…in");
      } else if (available^ <= 8) {
        Invalid("let…in…");
      } else {
        available := available^ - 8;
        let p' = abbreviate_pat(p);
        if (available^ > 3) {
          // " = "
          available := available^ - 3;
          let e1' = abbreviate_exp(e1);
          if (available^ > 4) {
            // " in "
            available := available^ - 4;
            let e2' = abbreviate_exp(e2);
            Let(p', e1', e2');
          } else {
            Let(
              p',
              e1',
              {
                ...e2,
                term: indet_term,
              },
            );
          };
        } else {
          Let(
            p',
            {
              ...e1,
              term: indet_term,
            },
            {
              ...e2,
              term: indet_term,
            },
          );
        };
      }

    | Theorem(p, e1, e2) =>
      if (available^ < 3) {
        indet_term;
      } else if (available^ <= 7) {
        Invalid("thm");
      } else if (available^ <= 9) {
        Invalid("theorem…");
      } else if (available^ <= 10) {
        Invalid("theorem…in");
      } else if (available^ <= 14) {
        Invalid("theorem…in…");
      } else {
        available := available^ - 12;
        let p' = abbreviate_pat(p);
        if (available^ > 3) {
          // " = "
          available := available^ - 3;
          let e1' = abbreviate_exp(e1);
          if (available^ > 4) {
            // " in "
            available := available^ - 4;
            let e2' = abbreviate_exp(e2);
            Let(p', e1', e2');
          } else {
            Let(
              p',
              e1',
              {
                ...e2,
                term: indet_term,
              },
            );
          };
        } else {
          Theorem(
            p',
            {
              ...e1,
              term: indet_term,
            },
            {
              ...e2,
              term: indet_term,
            },
          );
        };
      }

    | ProofObject(t) =>
      if (available^ < 8) {
        indet_term;
      } else if (available^ <= 12) {
        Invalid("proof_object");
      } else if (available^ <= 13) {
        Invalid("proof_object…");
      } else if (available^ <= 18) {
        Invalid("proof_object…end");
      } else {
        available := available^ - 16;
        let t' = abbreviate_exp(t);
        ProofObject(t');
      }

    | Use(t1, e1) =>
      if (available^ < 3) {
        indet_term;
      } else if (available^ <= 3) {
        Invalid("use");
      } else if (available^ <= 4) {
        Invalid("use…");
      } else if (available^ <= 6) {
        Invalid("use…in");
      } else if (available^ <= 8) {
        Invalid("use…in…");
      } else {
        available := available^ - 8;
        let t1' = abbreviate_typ(t1);
        if (available^ > 3) {
          // " = "
          available := available^ - 3;
          let e1' = abbreviate_exp(e1);
          Use(t1', e1');
        } else {
          Use(
            t1',
            {
              ...e1,
              term: indet_term,
            },
          );
        };
      }

    | TyAlias(tp, t, e) =>
      if (available^ < 4) {
        indet_term;
      } else if (available^ <= 4) {
        Invalid("type");
      } else if (available^ <= 6) {
        Invalid("type…");
      } else if (available^ <= 7) {
        Invalid("type…in");
      } else if (available^ <= 11) {
        Invalid("type…in…");
      } else {
        available := available^ - 8;
        let tp' = abbreviate_tpat(tp);
        if (available^ > 3) {
          // " = "
          available := available^ - 3;
          let t' = abbreviate_typ(t);
          if (available^ > 4) {
            // " in "
            available := available^ - 4;
            let e' = abbreviate_exp(e);
            TyAlias(tp', t', e');
          } else {
            TyAlias(
              tp',
              t',
              {
                ...e,
                term: indet_term,
              },
            );
          };
        } else {
          TyAlias(
            tp',
            {
              ...t,
              term: indet_term_typ,
            },
            {
              ...e,
              term: indet_term,
            },
          );
        };
      }

    | FixF(p, e, t) =>
      if (available^ < 3) {
        indet_term;
      } else if (available^ <= 3) {
        Invalid("fix");
      } else if (available^ <= 5) {
        Invalid("fix…");
      } else if (available^ <= 6) {
        Invalid("fix…→");
      } else if (available^ <= 7) {
        Invalid("fix…→…");
      } else {
        available := available^ - 7;
        let p' = abbreviate_pat(p);
        if (available^ > 4) {
          // " -> "
          available := available^ - 4;
          let e' = abbreviate_exp(e);
          FixF(p', e', t);
        } else {
          FixF(
            p',
            {
              ...e,
              term: indet_term,
            },
            t,
          );
        };
      }

    | Match(exp, pat_exp_pairs) =>
      if (available^ <= 3) {
        indet_term;
      } else if (available^ <= 4) {
        Invalid("case");
      } else if (available^ <= 7) {
        Invalid("case…");
      } else if (available^ <= 8) {
        Invalid("case…end");
      } else {
        available := available^ - 8;
        let exp' = abbreviate_exp(exp);
        let abbreviate_pair = ((p, e)) => (
          abbreviate_pat(p),
          abbreviate_exp(e),
        );
        let rec go = pairs =>
          switch (pairs) {
          | [] => []
          | [pair, ...pairs] =>
            let hd = abbreviate_pair(pair);
            if (available^ > 12) {
              available := available^ - 6; // "| " " => "
              [hd, ...go(pairs)];
              // } else if (available^ > 6) {
              //   available := available^ - 6; // "| " " => "
              //   [hd, (flat_ellipses_term_pat(), flat_ellipses_term())];
            } else if (available^ > 6) {
              [hd];
            } else {
              [];
            };
          };
        let pairs = go(pat_exp_pairs);
        Match(exp', pairs);
      }

    | TypAp(e, t) =>
      // <e> "@<"" <t> ">"
      if (available^ < 5) {
        indet_term;
      } else if (available^ <= 5) {
        Invalid("…@<…>");
      } else {
        available := available^ - 3; // "@<" ">"
        let e' = abbreviate_exp(e);
        if (available^ > 1) {
          available := available^ - 1;
          let t' = abbreviate_typ(t);
          TypAp(e', t');
        } else {
          TypAp(
            e',
            {
              ...t,
              term: indet_term_typ,
            },
          );
        };
      }

    | TypFun(tpat, e, name) =>
      if (available^ < 6) {
        indet_term;
      } else if (available^ <= 6) {
        Invalid("typfun");
      } else if (available^ <= 7) {
        Invalid("typfun…");
      } else if (available^ <= 8) {
        Invalid("typfun…→");
      } else if (available^ <= 9) {
        Invalid("typfun…→…");
      } else {
        available := available^ - 7;
        let tp' = abbreviate_tpat(tpat);
        if (available^ > 4) {
          // " -> "
          available := available^ - 4;
          let e' = abbreviate_exp(e);
          TypFun(tp', e', name);
        } else {
          TypFun(
            tp',
            {
              ...e,
              term: indet_term,
            },
            name,
          );
        };
      }

    | Forall(p, e) =>
      if (available^ < 6) {
        indet_term;
      } else if (available^ <= 6) {
        Invalid("forall");
      } else if (available^ <= 7) {
        Invalid("forall…");
      } else if (available^ <= 8) {
        Invalid("forall…→");
      } else if (available^ <= 9) {
        Invalid("forall…→…");
      } else {
        available := available^ - 7;
        let p' = abbreviate_pat(p);
        if (available^ > 4) {
          // " -> "
          available := available^ - 4;
          let e' = abbreviate_exp(e);
          Forall(p', e');
        } else {
          Forall(
            p',
            {
              ...e,
              term: indet_term,
            },
          );
        };
      }

    | Closure(env, exp) =>
      handle_unary(
        ~cost=1, // space between terms
        ~make_term=e' => Closure(env, e'),
        exp,
      )

    | MultiHole(things) =>
      if (available^ <= 1) {
        indet_term;
      } else {
        available := available^ - 1; // space
        MultiHole(List.map(abbreviate_any, things));
      }
    | Filter(_) => indet_term //TODO
    };
  rewrap(term);
}
and abbreviate_pat = (pat: Pat.t): Pat.t => {
  let rewrap = (term: Pat.term): Pat.t => {
    {
      ...pat,
      term,
    };
  };

  let wrap_or = (term, str): Pat.term =>
    if (available^ > String.length(str)) {
      available := available^ - String.length(str);
      term;
    } else {
      Invalid(abbreviate_str(available^, str));
    };

  let term: Pat.term =
    switch (pat.term) {
    | Wild => Wild
    | ExplicitNonlabel => ExplicitNonlabel
    | Var(v) => Var(abbreviate_str(available^, v))
    | Label(v) => Label(abbreviate_str(available^, v))
    | Atom(Int(n)) => wrap_or(Atom(Int(n)), Bigint.to_string(n))
    | Atom(Nat(n)) => wrap_or(Atom(Nat(n)), Bigint.to_string(n))
    | Atom(SInt(n)) => wrap_or(Atom(SInt(n)), string_of_int(n))
    | Atom(Float(f)) =>
      Invalid(abbreviate_str(available^, string_of_float(f)))
    | Atom(String(s)) =>
      let str = abbreviate_string_token(~min_len=available^, s);
      available := available^ - 2; // for quotes in printed representation
      Atom(String(str));
    | Atom(Bool(b)) => wrap_or(Atom(Bool(b)), string_of_bool(b))
    | Cons(p1, p2) =>
      if (available^ < 4) {
        indet_term_pat;
      } else if (available^ <= 4) {
        Invalid("…::…");
      } else {
        available := available^ - 2; // "::"
        let p1' = abbreviate_pat(p1);
        if (available^ > 1) {
          available := available^ - 1;
          let p2' = abbreviate_pat(p2);
          Cons(p1', p2');
        } else {
          Cons(
            p1',
            {
              ...p2,
              term: indet_term_pat,
            },
          );
        };
      }

    | Ap(p1, p2) =>
      if (available^ < 3) {
        indet_term_pat;
      } else if (available^ <= 3) {
        Invalid("(…)");
      } else {
        available := available^ - 2; // "()"
        let p1' = abbreviate_pat(p1);
        if (available^ > 1) {
          available := available^ - 1;
          let p2' = abbreviate_pat(p2);
          Ap(p1', p2');
        } else {
          Ap(
            p1',
            {
              ...p2,
              term: indet_term_pat,
            },
          );
        };
      }

    | Asc(p, t1) =>
      if (available^ < 3) {
        indet_term_pat;
      } else if (available^ <= 3) {
        Invalid("…:…");
      } else {
        available := available^ - 1; // ":"
        let p' = abbreviate_pat(p);
        if (available^ > 1) {
          available := available^ - 1;
          let t' = abbreviate_typ(t1);
          Asc(p', t');
        } else {
          Asc(p', t1);
        };
      }

    | ListLit(ps) =>
      if (available^ < 3) {
        indet_term_pat;
      } else if (available^ <= 3) {
        Invalid("[…]");
      } else {
        available := available^ - 2; // "[]"
        let ps' = List.map(abbreviate_pat, ps);
        ListLit(ps');
      }

    | Tuple(ps) =>
      if (available^ < 3) {
        indet_term_pat;
      } else if (available^ <= 3) {
        Invalid("(…)");
      } else {
        available := available^ - 2; // "()"
        let ps' = List.map(abbreviate_pat, ps);
        Tuple(ps');
      }

    | TupLabel(p1, p2) =>
      if (available^ <= 3) {
        indet_term_pat;
      } else {
        available := available^ - 3;
        TupLabel(abbreviate_pat(p1), abbreviate_pat(p2));
      }

    | MultiHole(things) =>
      if (available^ <= 1) {
        indet_term_pat;
      } else {
        available := available^ - 1; // space
        MultiHole(List.map(abbreviate_any, things));
      }

    | Invalid(str) =>
      let abbreviated =
        is_comment_token(str)
          ? abbreviate_string_token(~min_len=available^, str)
          : abbreviate_str(available^, str);
      Invalid(abbreviated);
    | EmptyHole => EmptyHole
    | Constructor(name, typ) =>
      if (available^ <= 1) {
        indet_term_pat;
      } else {
        available := available^ - 1; // space
        Constructor(name, typ);
      }
    | Parens(p)
    | Probe(p, _) =>
      if (available^ <= 3) {
        indet_term_pat;
      } else {
        available := available^ - 3; // "()"
        Parens(abbreviate_pat(p));
      }
    };
  rewrap(term);
}
and abbreviate_typ = (typ: Typ.t): Typ.t => {
  let rewrap = (term: Typ.term): Typ.t => {
    {
      ...typ,
      term,
    };
  };

  let handle_unary =
      (~cost: int, ~make_term: Exp.t => Typ.term, t: Exp.t): Typ.term =>
    if (available^ <= cost) {
      indet_term_typ;
    } else {
      available := available^ - cost;
      make_term(abbreviate_exp(t));
    };

  let term: Typ.term =
    switch (typ |> Typ.term_of) {
    | Unknown(prov) => Unknown(prov)
    | Atom(Int) =>
      if (available^ < 3) {
        indet_term_typ;
      } else {
        Atom(Int);
      }
    | Atom(SInt) =>
      if (available^ < 3) {
        indet_term_typ;
      } else {
        Atom(SInt);
      }
    | Atom(Nat) =>
      if (available^ < 3) {
        indet_term_typ;
      } else {
        Atom(Nat);
      }
    | Atom(Float) =>
      if (available^ < 5) {
        indet_term_typ;
      } else {
        Atom(Float);
      }
    | Atom(Bool) =>
      if (available^ < 4) {
        indet_term_typ;
      } else {
        Atom(Bool);
      }
    | Atom(String) =>
      if (available^ < 6) {
        indet_term_typ;
      } else {
        Atom(String);
      }
    | Var(v) => Var(abbreviate_str(available^, v))
    | ExplicitNonlabel => ExplicitNonlabel
    | Label(v) => Label(abbreviate_str(available^, v))
    | List(t) =>
      if (available^ <= 2) {
        indet_term_typ;
      } else {
        available := available^ - 2; // "[]"
        List(abbreviate_typ(t));
      }
    | Arrow(t1, t2) =>
      if (available^ <= 2) {
        indet_term_typ;
      } else {
        available := available^ - 2; // "->"
        let t1' = abbreviate_typ(t1);
        if (available^ > 0) {
          let t2' = abbreviate_typ(t2);
          Arrow(t1', t2');
        } else {
          Arrow(
            t1',
            {
              ...t2,
              term: indet_term_typ,
            },
          );
        };
      }
    | TupLabel(t1, t2) =>
      if (available^ <= 3) {
        indet_term_typ;
      } else {
        available := available^ - 3;
        TupLabel(abbreviate_typ(t1), abbreviate_typ(t2));
      }
    | ProdProjection(t1, t2) =>
      if (available^ <= 3) {
        indet_term_typ;
      } else {
        available := available^ - 3;
        ProdProjection(abbreviate_typ(t1), abbreviate_typ(t2));
      }
    | ProdExtension(t1, t2) =>
      if (available^ <= 3) {
        indet_term_typ;
      } else {
        available := available^ - 3; // "..."
        let t1' = abbreviate_typ(t1);
        if (available^ > 0) {
          let t2' = abbreviate_typ(t2);
          ProdExtension(t1', t2');
        } else {
          ProdExtension(
            t1',
            {
              ...t2,
              term: indet_term_typ,
            },
          );
        };
      }
    | Sum(ctors) =>
      if (available^ <= 1) {
        indet_term_typ;
      } else {
        //TODO: abbreviate these like tuples
        available := available^ - 1; // "+"
        let ctors' =
          ConstructorMap.map(t => Option.map(abbreviate_typ, t), ctors);
        Sum(ctors');
      }
    | Prod(ts) =>
      if (available^ <= 2) {
        indet_term_typ;
      } else {
        //TODO: abbreviate these like tuples
        available := available^ - 2; // "()"
        let ts' = List.map(abbreviate_typ, ts);
        Prod(ts');
      }
    | Parens(t) =>
      if (available^ <= 2) {
        indet_term_typ;
      } else {
        available := available^ - 2; // "()"
        Parens(abbreviate_typ(t));
      }
    | Rec(tp, t) =>
      if (available^ <= 3) {
        indet_term_typ;
      } else {
        available := available^ - 3; // "rec"
        let tp' = abbreviate_tpat(tp);
        if (available^ > 2) {
          available := available^ - 2; // "->"
          let t' = abbreviate_typ(t);
          Rec(tp', t');
        } else {
          Rec(
            tp',
            {
              ...t,
              term: indet_term_typ,
            },
          );
        };
      }
    | Poly(tp, t) =>
      if (available^ <= 6) {
        indet_term_typ;
      } else {
        available := available^ - 3; // "poly"
        let tp' = abbreviate_tpat(tp);
        if (available^ > 2) {
          available := available^ - 2; // "->"
          let t' = abbreviate_typ(t);
          Poly(tp', t');
        } else {
          Poly(
            tp',
            {
              ...t,
              term: indet_term_typ,
            },
          );
        };
      }
    | ProofOf(e) =>
      handle_unary(
        ~cost=13, // "proof_of " + " end"
        ~make_term=e' => ProofOf(e'),
        e,
      )
    };
  rewrap(term);
}
and abbreviate_tpat = (tpat: TPat.t): TPat.t => {
  let rewrap = term => {
    ...tpat,
    term,
  };
  let term =
    switch (tpat.term) {
    | EmptyHole => tpat.term
    | Invalid(str) =>
      let abbreviated =
        is_comment_token(str)
          ? abbreviate_string_token(~min_len=available^, str)
          : abbreviate_str(available^, str);
      Invalid(abbreviated);
    | Var(v) => Var(abbreviate_str(available^, v))
    | MultiHole(things) =>
      if (available^ <= 1) {
        indet_term_tpat;
      } else {
        available := available^ - 1; // space
        MultiHole(List.map(abbreviate_any, things));
      }
    };
  rewrap(term);
}
and abbreviate_any = (any: Any.t): Any.t =>
  switch (any) {
  | Exp(e) => Exp(abbreviate_exp(e))
  | Pat(p) => Pat(abbreviate_pat(p))
  | Typ(t) => Typ(abbreviate_typ(t))
  | TPat(tp) => TPat(abbreviate_tpat(tp))
  | Rul(_) => any
  | Any(_) => any
  };

let abbreviate_exp = (~available as a=12, exp: Exp.t): (Exp.t, int) => {
  available := a;
  let exp = abbreviate_exp(exp);
  let length_exp = a - available^;
  a < 0 || a <= 1 && length_exp > 1
    ? (flat_ellipses_term(), length_exp)
    : {
      (exp, length_exp);
    };
};

let abbreviate_pat = (~available as a=12, pat: Pat.t): (Pat.t, int) => {
  available := a;
  let pat = abbreviate_pat(pat);
  let length_pat = a - available^;
  a < 0 || a <= 1 && length_pat > 1
    ? (flat_ellipses_term_pat(), length_pat)
    : {
      (pat, length_pat);
    };
};
