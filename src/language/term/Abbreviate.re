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
  let ellipsis_cost: int = 1;

  /* Minimum display cost for an item when it renders structurally
     (not collapsed to bare ellipsis). Used to ensure items_to_show
     doesn't over-commit. Safety net will collapse to 1 (ellipsis)
     for any item that can't render at its minimum. */
  let min_display_cost = (item: Exp.t): int =>
    switch (item.term) {
    | Atom(String(_)) => 3 /* "…" */
    | TupLabel(_, _) => 5 /* …= … (label 1 + " = " 3 + value 1) */
    | Ap(Forward, {term: Constructor(_, _), _}, _) => 4 /* …(…) */
    | Parens(_) => 3 /* (…) */
    | ListLit([_, ..._]) => 3 /* […] */
    | Tuple([_, _, ..._]) => 1 /* bare tuple has no parens */
    | _ => 1
    };

  /* Display cost of "…+N" count annotation: …(1) + +(1) + digits */
  let count_annotation_cost = (remaining: int): int =>
    1 + 1 + String.length(string_of_int(remaining));

  let count_annotation_term = (remaining: int): Exp.t =>
    IdTagged.fresh(
      Invalid(flat_ellipses ++ "+" ++ string_of_int(remaining)): Exp.term,
    );

  /* Pre-compute how many items to show: choose largest c where
     c items + separators + trailing annotation fits in budget.
     Uses per-item "useful" cost (2x min for composites, 1x for atomics)
     to avoid committing to items that would render as empty shells. */
  let items_to_show = (~total: int, ~budget: int, ~useful_item: int): int => {
    let rec try_count = (c: int): int =>
      if (c <= 0) {
        0;
      } else {
        /* When c < total, there's a trailing annotation with a separator
           before it. So separators = c, not c-1. */
        let unshown = total - c;
        let seps =
          if (unshown > 0) {
            c * separator_cost;
          } else {
            (c - 1) * separator_cost;
          };
        let trailing =
          if (unshown > 0) {
            count_annotation_cost(unshown);
          } else {
            0;
          };
        let needed = c * useful_item + seps + trailing;
        if (needed <= budget) {
          c;
        } else {
          try_count(c - 1);
        };
      };
    try_count(total);
  };

  /* Greedy left-to-right: each item gets all available budget minus
     what's reserved (at min cost) for remaining items + trailing. */
  let rec consume =
          (
            items: list(Exp.t),
            ~items_left_to_show: int,
            ~total_remaining: int,
            ~min_per_item: int,
            ~abbreviate: Exp.t => Exp.t,
          )
          : list(Exp.t) =>
    switch (items) {
    | [] => []
    | _ when items_left_to_show <= 0 =>
      if (total_remaining > 0) {
        let cost = count_annotation_cost(total_remaining);
        available := available^ - cost;
        [count_annotation_term(total_remaining)];
      } else {
        [];
      }
    | [item, ...rest] =>
      let remaining_shown = items_left_to_show - 1;
      let unshown = total_remaining - items_left_to_show;
      let trailing_exists = unshown > 0;
      let has_more = remaining_shown > 0 || trailing_exists;

      /* Reserve: min budget for remaining shown items, seps between
         remaining outputs, and trailing annotation cost. */
      let remaining_outputs = remaining_shown + (trailing_exists ? 1 : 0);
      let seps_between_remaining = max(0, remaining_outputs - 1);
      let trailing_cost =
        trailing_exists ? count_annotation_cost(unshown) : 0;
      let reserve =
        remaining_shown
        * min_per_item
        + seps_between_remaining
        * separator_cost
        + trailing_cost;
      let sep_here = has_more ? separator_cost : 0;
      let this_budget = max(min_per_item, available^ - sep_here - reserve);

      let (abbreviated_item: Exp.t, _): (Exp.t, int) =
        AbbrevBudget.with_budget(~budget=this_budget, ~run=() =>
          abbreviate(item)
        );

      if (!has_more) {
        [abbreviated_item];
      } else {
        available := available^ - separator_cost;
        [
          abbreviated_item,
          ...consume(
               rest,
               ~items_left_to_show=items_left_to_show - 1,
               ~total_remaining=total_remaining - 1,
               ~min_per_item,
               ~abbreviate,
             ),
        ];
      };
    };

  let run = (~items: list(Exp.t), ~abbreviate: Exp.t => Exp.t): list(Exp.t) => {
    let count: int = List.length(items);
    if (count <= 0) {
      [];
    } else {
      let available_now: int = available^;
      /* Compute min_item from the maximum per-item minimum cost. */
      let min_item: int =
        List.fold_left(
          (acc, item) => max(acc, min_display_cost(item)),
          1,
          items,
        );
      /* Useful cost: same as min — items are shown when they can render
         at least their minimum structural form. For TupLabels (min=5) that's
         "…= …", for Parens (min=3) that's "(…)", for atomics it's the value. */
      let useful_item: int = min_item;
      /* Pre-decide how many items to show */
      let show_count: int =
        items_to_show(~total=count, ~budget=available_now, ~useful_item);
      if (show_count <= 0) {
        /* Tier 2: try showing items as bare ellipses (cost 1 each).
           This adds intermediates like (…, …+2) between (…) and (…= …, …+2)
           for composite items like TupLabels that have high structural min. */
        let fallback_count: int =
          min_item > 1
            ? items_to_show(
                ~total=count,
                ~budget=available_now,
                ~useful_item=1,
              )
            : 0;
        if (fallback_count <= 0) {
          /* Try count annotation (…+N) if budget allows, else bare … */
          let ann_cost = count_annotation_cost(count);
          if (available^ >= ann_cost) {
            available := available^ - ann_cost;
            [count_annotation_term(count)];
          } else {
            available := available^ - ellipsis_cost;
            [flat_ellipses_term()];
          };
        } else {
          consume(
            items,
            ~items_left_to_show=fallback_count,
            ~total_remaining=count,
            ~min_per_item=1,
            ~abbreviate,
          );
        };
      } else {
        consume(
          items,
          ~items_left_to_show=show_count,
          ~total_remaining=count,
          ~min_per_item=min_item,
          ~abbreviate,
        );
      };
    };
  };
};

/* Display width of the ellipsis character: 1 column on screen,
   even though it's 3 UTF-8 bytes. All budget accounting uses
   display characters, not bytes. */
let ellipsis_cost = 1;

let abbreviate_str = (min_len: int, s: string): string => {
  let len = String.length(s);
  if (len < 2 || len <= min_len) {
    /* String fits in budget or is too short to truncate */
    available := available^ - len;
    s;
  } else {
    /* Effective prefix: at least 0 chars */
    let prefix_len = max(0, min_len - 1);
    let truncated_display_len = prefix_len + ellipsis_cost;
    /* If truncation+ellipsis would be >= full string, just use full string */
    if (truncated_display_len >= len) {
      available := available^ - len;
      s;
    } else {
      let str =
        if (prefix_len > 0) {
          String.sub(s, 0, prefix_len) ++ flat_ellipses;
        } else {
          flat_ellipses;
        };
      available := available^ - truncated_display_len;
      str;
    };
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
  if (grapheme_len < 2 || grapheme_len <= min_len) {
    available := available^ - grapheme_len;
    s;
  } else {
    let prefix_count = max(0, min_len - 1);
    let truncated_display_len = prefix_count + ellipsis_cost;
    /* If truncation+ellipsis would be >= full string, just use full string */
    if (truncated_display_len >= grapheme_len) {
      available := available^ - grapheme_len;
      s;
    } else {
      let clusters: list(string) = Unicode.to_list(s);
      let prefix_clusters: list(string) =
        take_grapheme_prefix(prefix_count, clusters);
      let prefix: string = String.concat("", prefix_clusters);
      let truncated: string = prefix ++ flat_ellipses;
      available := available^ - truncated_display_len;
      truncated;
    };
  };
};

/* Abbreviate a label. Returns `Label(s) if the full label fits,
   or `Invalid(s) if truncated. Using Invalid for truncated labels
   avoids backtick-quoting by ExpToSegment (which quotes labels
   containing non-identifier chars like "…"), keeping display cost
   equal to string length with no quoting overhead. */
let abbreviate_label =
    (s: string)
    : [
        | `Label(string)
        | `Invalid(string)
      ] => {
  let len = String.length(s);
  let budget = available^;
  if (budget >= len) {
    available := available^ - len;
    `Label(s);
  } else {
    /* Truncated form: prefix + ellipsis, rendered as Invalid (no quoting) */
    let prefix_len = max(0, budget - ellipsis_cost);
    let cost = prefix_len + ellipsis_cost;
    let str =
      if (prefix_len > 0) {
        String.sub(s, 0, prefix_len) ++ flat_ellipses;
      } else {
        flat_ellipses;
      };
    available := available^ - cost;
    `Invalid(str);
  };
};

let indet_term: Exp.term = Invalid("?");
let indet_term_typ: Typ.term = Unknown(Internal);
let indet_term_pat: Pat.term = Invalid("?");
let indet_term_rul: Rul.term = Invalid("?");
let indet_term_tpat: TPat.term = Invalid("?");

let rec abbreviate_exp = (exp: Exp.t): Exp.t => {
  let initial = available^;
  if (initial <= 0) {
    available := available^ - ellipsis_cost;
    flat_ellipses_term();
  } else {
    let rewrap = (term: Exp.term): Exp.t => {
      {
        ...exp,
        term,
      };
    };
    let wrap_or = (term, str): Exp.term =>
      if (available^ >= String.length(str)) {
        available := available^ - String.length(str);
        term;
      } else {
        Invalid(abbreviate_str(available^, str));
      };

    let abbreviate_list_seq = (xs: list(Exp.t)): list(Exp.t) =>
      AbbrevSequence.run(~items=xs, ~abbreviate=abbreviate_exp);
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
        available := available^ - ellipsis_cost;
        Invalid(flat_ellipses);
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
        available := available^ - ellipsis_cost;
        Invalid(flat_ellipses);
      } else {
        available := available^ - cost;
        make_term(abbreviate_exp(e));
      };

    let term: Exp.term =
      switch (exp |> Exp.term_of) {
      | Fun(_p, _e, _, Some(s)) => Invalid("<" ++ s ++ ">")
      | Fun(_p, _e, _, None) => Invalid("<function>")
      | BuiltinFun(f) => Invalid("<" ++ f ++ ">")
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
        available := available^ - 2; // deduct quotes first for honest budget accounting
        if (available^ <= 0) {
          available := available^ - ellipsis_cost;
          Atom(String(flat_ellipses));
        } else {
          let str = abbreviate_string_token(~min_len=available^, s);
          Atom(String(str));
        };
      | Var(v) => Var(abbreviate_str(available^, v))
      | Label(v) =>
        switch (abbreviate_label(v)) {
        | `Label(s) => Label(s)
        | `Invalid(s) => Invalid(s)
        }
      | ExplicitNonlabel => ExplicitNonlabel
      | Constructor(c, t) => Constructor(abbreviate_str(available^, c), t)
      | LivelitName(v) => LivelitName(abbreviate_str(available^, v))

      // Other atomic cases
      | EmptyHole =>
        available := available^ - 1;
        EmptyHole;
      | ListLit([]) =>
        if (available^ < 2) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - 2;
          ListLit([]);
        }
      | Tuple([]) =>
        if (available^ < 2) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - 2;
          Tuple([]);
        }
      | Deferral(pos) =>
        available := available^ - 1;
        Deferral(pos);
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
        if (available^ < 3) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - 2; /* brackets */
          ListLit(abbreviate_list_seq(xs));
        }
      | Tuple([_, _, ..._] as xs) =>
        if (available^ < 1) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          /* No parens deduction: bare Tuple renders as comma-separated
             items without parens. Parens come from the Parens wrapper
             (which already deducts 2). */
          let abbreviated = abbreviate_tuple_seq(xs);
          /* Avoid producing single-element tuples from multi-element inputs,
             since ExpToSegment renders Tuple([e]) as (_ = e) which can be
             longer than the abbreviated content. Extract the text from the
             single Invalid element to preserve count annotations (…+N). */
          switch (abbreviated) {
          | [{term: Invalid(s), _}] => Invalid(s)
          | [single] => single.term
          | _ => Tuple(abbreviated)
          };
        }
      | TupLabel(e1, e2) =>
        /* Min TupLabel: label (1) + " = " (3) + value (1) = 5.
           Truncated labels use Invalid (no backtick quoting). */
        if (available^ < 5) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - 3; /* " = " */
          let remaining: int = available^;
          /* Give label its estimated length if possible, else at least 1. */
          let label_est = AbbrevBudget.label_estimated_length(~label=e1);
          let label_budget =
            if (remaining >= label_est) {
              label_est;
            } else {
              max(1, remaining - 1);
            };
          let label_budget = min(label_budget, remaining - 1);
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
        if (available^ < 4) {
          /* Min: 1 name + ( + 1 arg + ) = 4 */
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          let name_budget = available^ - 3; /* reserve ( + 1 arg + ) */
          let (konst, _) =
            AbbrevBudget.with_budget(~budget=name_budget, ~run=() =>
              abbreviate_exp(konst)
            );
          available := available^ - 2; /* parens */
          let arg = abbreviate_exp(arg);
          Ap(Forward, konst, arg);
        }
      | Parens(e) =>
        if (available^ < 3) {
          /* Min: ( + 1 content + ) = 3 */
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - 2;
          Parens(abbreviate_exp(e));
        }

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
          ~cost=2, // "! " (op + space)
          ~make_term=e' => UnOp(Bool(Not), e'),
          e,
        )
      | UnOp(SInt(Minus), e) =>
        handle_unary(
          ~cost=2, // "- " (op + space)
          ~make_term=e' => UnOp(SInt(Minus), e'),
          e,
        )
      | UnOp(Float(Minus), e) =>
        handle_unary(
          ~cost=2, // "~. " (op + space) -- actually "-. " is 3 chars
          ~make_term=e' => UnOp(Float(Minus), e'),
          e,
        )
      | UnOp(Nat(Minus), e) =>
        handle_unary(
          ~cost=2, // "- " (op + space)
          ~make_term=e' => UnOp(Nat(Minus), e'),
          e,
        )
      | UnOp(Int(Minus), e) =>
        handle_unary(
          ~cost=2, // "- " (op + space)
          ~make_term=e' => UnOp(Int(Minus), e'),
          e,
        )
      | UnOp(Meta(Unquote), e) =>
        handle_unary(
          ~cost=2, // "$ " (op + space)
          ~make_term=e' => UnOp(Meta(Unquote), e'),
          e,
        )

      // Binary operations
      | BinOp(op, e1, e2) =>
        let op_str = Operators.bin_op_to_string(op);
        /* Display cost: " op " = op_len + 2 spaces */
        let op_cost = String.length(op_str) + 2;

        if (available^ < op_cost + 2) {
          /* Not enough for op + minimum 1-char operands */
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - op_cost;
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else if (available^ < 4) {
          available := available^ - 3;
          Invalid("let");
        } else if (available^ < 6) {
          available := available^ - 4;
          Invalid("let…");
        } else if (available^ < 7) {
          available := available^ - 6;
          Invalid("let…in");
        } else if (available^ < 14) {
          available := available^ - 7;
          Invalid("let…in…");
        } else {
          /* Full form: "let " (4) + p + " = " (3) + e1 + " in " (4) + e2 */
          available := available^ - 11; /* 4 + 3 + 4 overhead */
          let pool = available^;
          let budgets = AbbrevBudget.split_evenly(~total=pool, ~parts=3);
          let (p', _) =
            AbbrevBudget.with_budget(~budget=List.nth(budgets, 0), ~run=() =>
              abbreviate_pat(p)
            );
          let (e1', _) =
            AbbrevBudget.with_budget(~budget=List.nth(budgets, 1), ~run=() =>
              abbreviate_exp(e1)
            );
          let (e2', _) =
            AbbrevBudget.with_budget(~budget=List.nth(budgets, 2), ~run=() =>
              abbreviate_exp(e2)
            );
          Let(p', e1', e2');
        }

      | Theorem(p, e1, e2) =>
        if (available^ < 3) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else if (available^ < 8) {
          available := available^ - 3;
          Invalid("thm");
        } else if (available^ < 10) {
          available := available^ - 8;
          Invalid("theorem…");
        } else if (available^ < 11) {
          available := available^ - 10;
          Invalid("theorem…in");
        } else if (available^ < 18) {
          available := available^ - 11;
          Invalid("theorem…in…");
        } else {
          /* Full: "theorem " (8) + p + " = " (3) + e1 + " in " (4) + e2 */
          available := available^ - 15;
          let pool = available^;
          let budgets = AbbrevBudget.split_evenly(~total=pool, ~parts=3);
          let (p', _) =
            AbbrevBudget.with_budget(~budget=List.nth(budgets, 0), ~run=() =>
              abbreviate_pat(p)
            );
          let (e1', _) =
            AbbrevBudget.with_budget(~budget=List.nth(budgets, 1), ~run=() =>
              abbreviate_exp(e1)
            );
          let (e2', _) =
            AbbrevBudget.with_budget(~budget=List.nth(budgets, 2), ~run=() =>
              abbreviate_exp(e2)
            );
          Theorem(p', e1', e2');
        }

      | ProofObject(t) =>
        if (available^ < 8) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - 1; // space
          MultiHole(List.map(abbreviate_any, things));
        }
      | Filter(_) =>
        //TODO

        available := available^ - ellipsis_cost;
        Invalid(flat_ellipses);
      | Projector(data, e) => Projector(data, abbreviate_exp(e))
      };
    let result = rewrap(term);
    /* Safety net: if the form overspent, retry with budget-1.
       This ensures monotonicity: output at budget B is at least as wide
       as output at budget B-1, since we fall back to exactly that. */
    if (available^ < 0) {
      /* Restore available to initial before retrying */
      available := initial;
      let retry_budget = initial - 1;
      if (retry_budget <= 0) {
        available := initial - ellipsis_cost;
        flat_ellipses_term();
      } else {
        let (retried, _) =
          AbbrevBudget.with_budget(~budget=retry_budget, ~run=() =>
            abbreviate_exp(exp)
          );
        retried;
      };
    } else {
      result;
    };
  };
}
and abbreviate_pat = (pat: Pat.t): Pat.t => {
  let initial = available^;
  if (initial <= 0) {
    available := available^ - ellipsis_cost;
    flat_ellipses_term_pat();
  } else {
    let rewrap = (term: Pat.term): Pat.t => {
      {
        ...pat,
        term,
      };
    };

    let wrap_or = (term, str): Pat.term =>
      if (available^ >= String.length(str)) {
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
      | Label(v) =>
        switch (abbreviate_label(v)) {
        | `Label(s) => Label(s)
        | `Invalid(s) => Invalid(s)
        }
      | Atom(Int(n)) => wrap_or(Atom(Int(n)), Bigint.to_string(n))
      | Atom(Nat(n)) => wrap_or(Atom(Nat(n)), Bigint.to_string(n))
      | Atom(SInt(n)) => wrap_or(Atom(SInt(n)), string_of_int(n))
      | Atom(Float(f)) =>
        Invalid(abbreviate_str(available^, string_of_float(f)))
      | Atom(String(s)) =>
        available := available^ - 2; // deduct quotes first for honest budget accounting
        if (available^ <= 0) {
          available := available^ - ellipsis_cost;
          Atom(String(flat_ellipses));
        } else {
          let str = abbreviate_string_token(~min_len=available^, s);
          Atom(String(str));
        };
      | Atom(Bool(b)) => wrap_or(Atom(Bool(b)), string_of_bool(b))
      | Cons(p1, p2) =>
        if (available^ < 4) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else if (available^ <= 3) {
          Invalid("[…]");
        } else {
          available := available^ - 2; // "[]"
          let ps' = List.map(abbreviate_pat, ps);
          ListLit(ps');
        }

      | Tuple(ps) =>
        if (available^ < 3) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else if (available^ <= 3) {
          Invalid("(…)");
        } else {
          available := available^ - 2; // "()"
          let ps' = List.map(abbreviate_pat, ps);
          Tuple(ps');
        }

      | TupLabel(p1, p2) =>
        if (available^ < 5) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - 3;
          TupLabel(abbreviate_pat(p1), abbreviate_pat(p2));
        }

      | MultiHole(things) =>
        if (available^ <= 1) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
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
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - 1; // space
          Constructor(name, typ);
        }
      | Parens(p) =>
        if (available^ <= 3) {
          available := available^ - ellipsis_cost;
          Invalid(flat_ellipses);
        } else {
          available := available^ - 3; // "()"
          Parens(abbreviate_pat(p));
        }
      | Projector(data, p) => Projector(data, abbreviate_pat(p))
      };
    let result = rewrap(term);
    if (available^ < 0) {
      available := initial;
      let retry_budget = initial - 1;
      if (retry_budget <= 0) {
        available := initial - ellipsis_cost;
        flat_ellipses_term_pat();
      } else {
        let (retried, _) =
          AbbrevBudget.with_budget(~budget=retry_budget, ~run=() =>
            abbreviate_pat(pat)
          );
        retried;
      };
    } else {
      result;
    };
  };
}
and abbreviate_typ = (typ: Typ.t): Typ.t => {
  let initial = available^;
  if (initial <= 0) {
    available := available^ - 1;
    {
      ...typ,
      term: indet_term_typ,
    };
  } else {
    let rewrap = (term: Typ.term): Typ.t => {
      {
        ...typ,
        term,
      };
    };

    let handle_unary =
        (~cost: int, ~make_term: Exp.t => Typ.term, t: Exp.t): Typ.term =>
      if (available^ <= cost) {
        available := available^ - 1;
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
          available := available^ - 1;
          indet_term_typ;
        } else {
          Atom(Int);
        }
      | Atom(SInt) =>
        if (available^ < 3) {
          available := available^ - 1;
          indet_term_typ;
        } else {
          Atom(SInt);
        }
      | Atom(Nat) =>
        if (available^ < 3) {
          available := available^ - 1;
          indet_term_typ;
        } else {
          Atom(Nat);
        }
      | Atom(Float) =>
        if (available^ < 5) {
          available := available^ - 1;
          indet_term_typ;
        } else {
          Atom(Float);
        }
      | Atom(Bool) =>
        if (available^ < 4) {
          available := available^ - 1;
          indet_term_typ;
        } else {
          Atom(Bool);
        }
      | Atom(String) =>
        if (available^ < 6) {
          available := available^ - 1;
          indet_term_typ;
        } else {
          Atom(String);
        }
      | Var(v) => Var(abbreviate_str(available^, v))
      | ExplicitNonlabel => ExplicitNonlabel
      | Label(v) =>
        /* Typ has no Invalid constructor; keep as Label even when truncated.
           Backtick quoting in type abbreviations is a minor cosmetic issue. */
        switch (abbreviate_label(v)) {
        | `Label(s)
        | `Invalid(s) => Label(s)
        }
      | List(t) =>
        if (available^ <= 2) {
          available := available^ - 1;
          indet_term_typ;
        } else {
          available := available^ - 2; // "[]"
          List(abbreviate_typ(t));
        }
      | Arrow(t1, t2) =>
        if (available^ <= 2) {
          available := available^ - 1;
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
        if (available^ < 5) {
          available := available^ - 1;
          indet_term_typ;
        } else {
          available := available^ - 3;
          TupLabel(abbreviate_typ(t1), abbreviate_typ(t2));
        }
      | ProdProjection(t1, t2) =>
        if (available^ <= 3) {
          available := available^ - 1;
          indet_term_typ;
        } else {
          available := available^ - 3;
          ProdProjection(abbreviate_typ(t1), abbreviate_typ(t2));
        }
      | ProdExtension(t1, t2) =>
        if (available^ <= 3) {
          available := available^ - 1;
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
          available := available^ - 1;
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
          available := available^ - 1;
          indet_term_typ;
        } else {
          //TODO: abbreviate these like tuples
          available := available^ - 2; // "()"
          let ts' = List.map(abbreviate_typ, ts);
          Prod(ts');
        }
      | Parens(t) =>
        if (available^ <= 2) {
          available := available^ - 1;
          indet_term_typ;
        } else {
          available := available^ - 2; // "()"
          Parens(abbreviate_typ(t));
        }
      | Rec(tp, t) =>
        if (available^ <= 3) {
          available := available^ - 1;
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
          available := available^ - 1;
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
      | Projector(data, t) => Projector(data, abbreviate_typ(t))
      };
    let result = rewrap(term);
    if (available^ < 0) {
      available := initial;
      let retry_budget = initial - 1;
      if (retry_budget <= 0) {
        available := initial - 1;
        {
          ...typ,
          term: indet_term_typ,
        };
      } else {
        let (retried, _) =
          AbbrevBudget.with_budget(~budget=retry_budget, ~run=() =>
            abbreviate_typ(typ)
          );
        retried;
      };
    } else {
      result;
    };
  };
}
and abbreviate_tpat = (tpat: TPat.t): TPat.t =>
  if (available^ <= 0) {
    available := available^ - ellipsis_cost;
    {
      ...tpat,
      term: Invalid(flat_ellipses),
    };
  } else {
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
  if (a < 0) {
    (flat_ellipses_term(), ellipsis_cost);
  } else {
    (exp, length_exp);
  };
};

let abbreviate_pat = (~available as a=12, pat: Pat.t): (Pat.t, int) => {
  available := a;
  let pat = abbreviate_pat(pat);
  let length_pat = a - available^;
  if (a < 0) {
    (flat_ellipses_term_pat(), ellipsis_cost);
  } else {
    (pat, length_pat);
  };
};
