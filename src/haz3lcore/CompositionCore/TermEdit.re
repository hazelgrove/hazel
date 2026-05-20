/* TermEdit: Term-level syntax transformations for structural editing.

      Instead of manipulating the zipper/segment directly (which has sort-context
      issues for module items), we:
      1. Get the full program term from the zipper
      2. Modify the term tree (splice in/out sub-terms)
      3. Convert back to a segment via ExpToSegment with PreserveExact
      4. Create a new zipper from the modified segment

      This approach is sort-correct by construction and handles modules cleanly.
      PreserveExact reads stored secondary (whitespace/newlines) from each term's
      annotation, preserving the original formatting of unmodified code.
      Freshly constructed nodes must have their secondary populated explicitly.
   */

open Util;
open Language;

/* Round-trip settings: use PreserveExact to preserve original whitespace.
   MakeTerm populates each term's annotation.secondary with (before, after)
   whitespace runs from the original segment. PreserveExact emits these
   verbatim. Freshly constructed nodes must have their secondary populated
   explicitly (via fresh_exp_with_secondary or ensure_leading_secondary).
   inline: true avoids generating additional heuristic newlines that
   would double up with stored secondary. */
let roundtrip_settings: ExpToSegment.Settings.t = {
  secondary: PreserveExact,
  parenthesization: Structural,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
};

/* Create a newline Secondary token */
let mk_newline = (): Secondary.t => {
  id: Id.mk(),
  content: Whitespace("\n"),
};

/* Create a space Secondary token */
let mk_space = (): Secondary.t => {
  id: Id.mk(),
  content: Whitespace(" "),
};

/* Create an Exp.t with secondary annotation (before, after whitespace runs).
   Use this for programmatically constructed nodes that need whitespace
   around them when rendered with PreserveExact mode. */
let fresh_exp_with_secondary =
    (~before=[], ~after=[], term: TermBase.exp_term): Exp.t => {
  let e = Exp.fresh(term);
  {
    ...e,
    annotation: {
      ...e.annotation,
      secondary: (before, after),
    },
  };
};

/* Ensure an expression has leading secondary (whitespace before it).
   If it already has before-secondary, return unchanged.
   Otherwise add the default (a space). */
let ensure_leading_secondary = (~default=mk_space(), e: Exp.t): Exp.t => {
  let (before, after) = e.annotation.secondary;
  switch (before) {
  | [] => {
      ...e,
      annotation: {
        ...e.annotation,
        secondary: ([default], after),
      },
    }
  | _ => e
  };
};

/* Convert a term back to a zipper via round-trip */
let term_to_zipper = (term: Exp.t): Zipper.t => {
  let segment =
    ExpToSegment.exp_to_segment(~settings=roundtrip_settings, term);
  Zipper.unzip(segment);
};

/* Replace a Module's items list in a term tree using Exp.map_term.
   Walks the entire tree looking for the module with the given ID
   and replaces its items list. */
let replace_module_items =
    (target_module_id: Id.t, new_items: list(Mod.t), term: Exp.t): Exp.t => {
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        if (Id.equal(Exp.rep_id(e), target_module_id)) {
          switch (Exp.term_of(e)) {
          | Module(_) => {
              ...e,
              term: Module(new_items),
            }
          | _ => continue(e)
          };
        } else {
          continue(e);
        },
    term,
  );
};

/* Find which module contains the target item, given the item's ID.
   Returns (module_exp_id, module_items, item_index). */
let find_module_containing_item =
    (target_item_id: Id.t, term: Exp.t): option((Id.t, list(Mod.t), int)) => {
  let result = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Module(items) =>
            switch (
              ListUtil.findi_opt(
                (item: Mod.t) => Id.equal(Mod.rep_id(item), target_item_id),
                items,
              )
            ) {
            | Some((idx, _)) =>
              result := Some((Exp.rep_id(e), items, idx));
              e;
            | None => continue(e)
            }
          | _ => continue(e)
          }
        },
      term,
    );
  result^;
};

/* Convert an expression "let x = 42" to a ModLet item.
   Parses the code, and if it's a Let(pat, def, _), extracts pat+def into ModLet.
   Also handles type T = ... → ModType, and bare expressions → ModExp. */
let exp_to_mod_item = (code: string): option(Mod.t) => {
  switch (Parser.to_term(code, ~root=Exp)) {
  | Some(term) =>
    let item_term: TermBase.Mod.term =
      switch (Exp.term_of(term)) {
      | Let(pat, def, _) => ModLet(pat, def)
      | TyAlias(tpat, tdef, _) => ModType(tpat, tdef)
      | _ => ModExp(term) /* Bare expression becomes ModExp */
      };
    /* Add a space before the item so it renders as "; let b = 2"
       rather than ";let b = 2" when round-tripped */
    let space: Secondary.t = {
      id: Id.mk(),
      content: Whitespace(" "),
    };
    Some(IdTagged.mk([Id.mk()], ([space], []), item_term));
  | None => None
  };
};

/* Delete a module item by index from the items list.
   Returns the modified items list (no hole left). */
let delete_item = (items: list(Mod.t), idx: int): list(Mod.t) =>
  List.filteri((i, _) => i != idx, items);

/* Insert a module item at a position in the items list.
   Left = before idx, Right = after idx.
   When inserting at the end, adds trailing secondary (space before })
   to the new last item. */
let insert_item =
    (items: list(Mod.t), idx: int, new_item: Mod.t, d: Direction.t)
    : list(Mod.t) => {
  let insert_at = d == Left ? idx : idx + 1;
  let (before, after) = ListUtil.split_n(insert_at, items);
  /* If inserting at the end, add a trailing space to the new item
     so there's whitespace before the closing } delimiter */
  let new_item =
    switch (after) {
    | [] =>
      let (item_before, _) = new_item.annotation.secondary;
      let space: Secondary.t = {
        id: Id.mk(),
        content: Whitespace(" "),
      };
      IdTagged.mk(
        new_item.annotation.ids,
        (item_before, [space]),
        new_item.term,
      );
    | _ => new_item
    };
  before @ [new_item] @ after;
};

/* Copy secondary from one Mod item to another. */
let copy_mod_secondary = (from: Mod.t, to_: Mod.t): Mod.t => {
  let (before, after) = from.annotation.secondary;
  IdTagged.mk(to_.annotation.ids, (before, after), to_.term);
};

/* Replace a module item at a position in the items list.
   Copies secondary from original item to preserve positional whitespace. */
let replace_item =
    (items: list(Mod.t), idx: int, new_item: Mod.t): list(Mod.t) =>
  List.mapi(
    (i, item) => i == idx ? copy_mod_secondary(item, new_item) : item,
    items,
  );

/* --- High-level edit operations --- */

/* Delete a module item cleanly (no hole left).
   target_item_id: the ID of the ModLet/ModType item to delete.
   Returns the modified zipper, or None if the item wasn't found. */
let module_delete = (z: Zipper.t, target_item_id: Id.t): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_module_containing_item(target_item_id, term)) {
  | Some((module_id, items, idx)) =>
    let new_items = delete_item(items, idx);
    let new_term = replace_module_items(module_id, new_items, term);
    Some(term_to_zipper(new_term));
  | None => None
  };
};

/* Insert a new item into a module.
   target_item_id: the ID of the reference item (insert before/after it).
   code: the text to parse as a module item.
   d: Left = before, Right = after. */
let module_insert =
    (z: Zipper.t, target_item_id: Id.t, code: string, d: Direction.t)
    : option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_module_containing_item(target_item_id, term)) {
  | Some((module_id, items, idx)) =>
    switch (exp_to_mod_item(code)) {
    | Some(new_item) =>
      let new_items = insert_item(items, idx, new_item, d);
      let new_term = replace_module_items(module_id, new_items, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

/* Replace a module item's binding clause.
   target_item_id: the ID of the item to replace.
   code: the full new binding clause text (e.g. "let y = 42"). */
let module_update_binding =
    (z: Zipper.t, target_item_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_module_containing_item(target_item_id, term)) {
  | Some((module_id, items, idx)) =>
    switch (exp_to_mod_item(code)) {
    | Some(new_item) =>
      let new_items = replace_item(items, idx, new_item);
      let new_term = replace_module_items(module_id, new_items, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

/* Check if a target ID is inside a Module expression */
let is_module_item = (z: Zipper.t, target_id: Id.t): bool => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_module_containing_item(target_id, term)) {
  | Some(_) => true
  | None => false
  };
};

/* Find the module item ID that contains or matches a given expression ID.
   This bridges selector-focused IDs (which target sub-expressions like defs)
   to module item IDs needed by module_insert/module_delete. */
let find_module_item_id = (z: Zipper.t, exp_id: Id.t): option(Id.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  /* First check if exp_id IS a module item ID directly */
  switch (find_module_containing_item(exp_id, term)) {
  | Some(_) => Some(exp_id)
  | None =>
    /* Otherwise scan all modules to find an item whose sub-expression
       (pat/def) has the given ID */
    let result = ref(None);
    let _ =
      Exp.map_term(
        ~f_exp=
          (continue, e) => {
            switch (Exp.term_of(e)) {
            | Module(items) =>
              List.iter(
                (item: Mod.t) =>
                  switch (item.term) {
                  | ModLet(pat, def) =>
                    if (Id.equal(Pat.rep_id(pat), exp_id)
                        || Id.equal(Exp.rep_id(def), exp_id)) {
                      result := Some(Mod.rep_id(item));
                    }
                  | ModuleMod(mpat, def) =>
                    if (Id.equal(IdTagged.rep_id(mpat), exp_id)
                        || Id.equal(Exp.rep_id(def), exp_id)) {
                      result := Some(Mod.rep_id(item));
                    }
                  | ModType(tpat, _tdef) =>
                    if (Id.equal(TPat.rep_id(tpat), exp_id)) {
                      result := Some(Mod.rep_id(item));
                    }
                  | ModExp(e') =>
                    if (Id.equal(Exp.rep_id(e'), exp_id)) {
                      result := Some(Mod.rep_id(item));
                    }
                  | _ => ()
                  },
                items,
              );
              continue(e);
            | _ => continue(e)
            }
          },
        term,
      );
    result^;
  };
};

/* --- General-purpose term-level edit operations --- */

/* Extract the effective leading (before) secondary from an expression.
   For compound forms like BinOp where the root's before-secondary is empty
   (because MakeTerm assigns it to the leftmost leaf), this walks left
   to find the actual leading whitespace. */
let rec leading_secondary_of = (e: Exp.t): list(Secondary.t) => {
  let (before, _) = e.annotation.secondary;
  switch (before) {
  | [_, ..._] => before
  | [] =>
    switch (Exp.term_of(e)) {
    | BinOp(_, e1, _)
    | Seq(e1, _)
    | Cons(e1, _)
    | ListConcat(e1, _) => leading_secondary_of(e1)
    | Ap(_, e1, _) => leading_secondary_of(e1)
    | Tuple([e1, ..._])
    | ListLit([e1, ..._]) => leading_secondary_of(e1)
    | _ => []
    }
  };
};

/* Extract the effective trailing (after) secondary from an expression.
   Symmetric to leading_secondary_of: walks rightward for compound forms
   where the root's after-secondary is empty but the rightmost leaf has it. */
let rec trailing_secondary_of = (e: Exp.t): list(Secondary.t) => {
  let (_, after) = e.annotation.secondary;
  switch (after) {
  | [_, ..._] => after
  | [] =>
    switch (Exp.term_of(e)) {
    | BinOp(_, _, e2)
    | Seq(_, e2)
    | Cons(_, e2)
    | ListConcat(_, e2) => trailing_secondary_of(e2)
    | Ap(_, _, e2) => trailing_secondary_of(e2)
    | Tuple(es)
    | ListLit(es) =>
      switch (ListUtil.last_opt(es)) {
      | Some(last) => trailing_secondary_of(last)
      | None => []
      }
    /* Prefix forms: body/else is the rightmost content after the tile */
    | Fun(_, body, _, _)
    | FixF(_, body, _)
    | TypFun(_, body, _)
    | Forall(_, body)
    | Filter(_, body)
    | Let(_, _, body)
    | TyAlias(_, _, body)
    | ModuleExp(_, _, body) => trailing_secondary_of(body)
    | If(_, _, else_) => trailing_secondary_of(else_)
    | _ => []
    }
  };
};

/* Copy positional secondary (whitespace) from one term to another.
   When replacing a node in the tree, the replacement should inherit the
   original's secondary so PreserveExact mode maintains correct spacing.
   For compound forms where leading/trailing whitespace lives on a child
   (e.g., BinOp root has empty secondary), extracts via walking left/right. */
let copy_exp_secondary = (from: Exp.t, to_: Exp.t): Exp.t => {
  let (from_before, from_after) = from.annotation.secondary;
  let before =
    switch (from_before) {
    | [_, ..._] => from_before
    | [] => leading_secondary_of(from)
    };
  let after =
    switch (from_after) {
    | [_, ..._] => from_after
    | [] => trailing_secondary_of(from)
    };
  {
    ...to_,
    annotation: {
      ...to_.annotation,
      secondary: (before, after),
    },
  };
};
let copy_pat_secondary = (from: Pat.t, to_: Pat.t): Pat.t => {
  ...to_,
  annotation: {
    ...to_.annotation,
    secondary: from.annotation.secondary,
  },
};
let copy_typ_secondary = (from: Typ.t, to_: Typ.t): Typ.t => {
  ...to_,
  annotation: {
    ...to_.annotation,
    secondary: from.annotation.secondary,
  },
};
let copy_tpat_secondary = (from: TPat.t, to_: TPat.t): TPat.t => {
  ...to_,
  annotation: {
    ...to_.annotation,
    secondary: from.annotation.secondary,
  },
};

/* Replace a sub-expression by ID anywhere in the term tree.
   The replacement function receives the matched expression and returns
   the new expression to substitute. Automatically copies positional
   secondary from the original to the replacement when the ID changes,
   preserving whitespace context for PreserveExact rendering. */
let replace_exp_by_id =
    (target_id: Id.t, f: Exp.t => Exp.t, term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        if (Id.equal(Exp.rep_id(e), target_id)) {
          let result = f(e);
          /* Copy secondary when IDs differ (fresh replacement node).
             When IDs match (spread/mutation), secondary already correct. */
          if (Id.equal(Exp.rep_id(result), Exp.rep_id(e))) {
            result;
          } else {
            copy_exp_secondary(e, result);
          };
        } else {
          continue(e);
        },
    term,
  );

/* Replace a sub-pattern by ID. Walks the term tree and within any
   expression that contains the target pattern, replaces it.
   Copies secondary from original to preserve positional whitespace. */
let replace_pat_by_id = (target_id: Id.t, new_pat: Pat.t, term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_pat=
      (continue, p) =>
        if (Id.equal(Pat.rep_id(p), target_id)) {
          copy_pat_secondary(p, new_pat);
        } else {
          continue(p);
        },
    term,
  );

/* Replace a sub-type by ID.
   Copies secondary from original to preserve positional whitespace. */
let replace_typ_by_id = (target_id: Id.t, new_typ: Typ.t, term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_typ=
      (continue, t) =>
        if (Id.equal(Typ.rep_id(t), target_id)) {
          copy_typ_secondary(t, new_typ);
        } else {
          continue(t);
        },
    term,
  );

/* Replace a sub-tpat by ID.
   Copies secondary from original to preserve positional whitespace. */
let replace_tpat_by_id =
    (target_id: Id.t, new_tpat: TPat.t, term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_tpat=
      (continue, tp) =>
        if (Id.equal(TPat.rep_id(tp), target_id)) {
          copy_tpat_secondary(tp, new_tpat);
        } else {
          continue(tp);
        },
    term,
  );

/* Parse code as an expression term.
   Returns None if the code has parse errors (unmatched delimiters,
   invalid tokens, or malformed expressions). */
let parse_exp = (code: string): option(Exp.t) =>
  switch (Parser.to_zipper(~root=Exp, code)) {
  | Some(z) =>
    /* Check for unmatched delimiters in backpack */
    let backpack = Zipper.local_backpack(z);
    switch (backpack) {
    | [_, ..._] => None
    | [] =>
      let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
      /* Check for Invalid/MultiHole in the parsed term */
      let has_errors = ref(false);
      let _ =
        Exp.map_term(
          ~f_exp=
            (continue, e) =>
              switch (Exp.term_of(e)) {
              | Invalid(_)
              | MultiHole(_) =>
                has_errors := true;
                e;
              | _ => continue(e)
              },
          term,
        );
      if (has_errors^) {
        None;
      } else {
        Some(term);
      };
    };
  | None => None
  };

/* Parse code as a pattern term */
let parse_pat = (code: string): option(Pat.t) =>
  switch (Parser.to_term("let " ++ code ++ " = 0 in 0", ~root=Exp)) {
  | Some(term) =>
    switch (Exp.term_of(term)) {
    | Let(pat, _, _) => Some(pat)
    | _ => None
    }
  | None => None
  };

/* Parse code as a type term */
let parse_typ = (code: string): option(Typ.t) =>
  switch (Parser.to_term("let x : " ++ code ++ " = 0 in 0", ~root=Exp)) {
  | Some(term) =>
    switch (Exp.term_of(term)) {
    | Let(pat, _, _) =>
      switch (Pat.term_of(pat)) {
      | Asc(_, typ) => Some(typ)
      | _ => None
      }
    | _ => None
    }
  | None => None
  };

/* --- Case arm operations --- */

/* A case arm is (Pat.t, Exp.t) — pattern + body. */
type case_arm = (Pat.t, Exp.t);

/* Replace the arms list of a Match expression by ID. */
let replace_match_arms =
    (target_match_id: Id.t, new_arms: list(case_arm), term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        if (Id.equal(Exp.rep_id(e), target_match_id)) {
          switch (Exp.term_of(e)) {
          | Match(scrutinee, _) => {
              ...e,
              term: Match(scrutinee, new_arms),
            }
          | _ => continue(e)
          };
        } else {
          continue(e);
        },
    term,
  );

/* Find the Match expression containing an arm whose body has the given ID.
   Returns (match_id, scrutinee, arms, arm_index). */
let find_match_containing_arm_by_body =
    (target_body_id: Id.t, term: Exp.t)
    : option((Id.t, Exp.t, list(case_arm), int)) => {
  let result = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Match(scrutinee, arms) =>
            switch (
              ListUtil.findi_opt(
                ((_, body)) => Id.equal(Exp.rep_id(body), target_body_id),
                arms,
              )
            ) {
            | Some((idx, _)) =>
              result := Some((Exp.rep_id(e), scrutinee, arms, idx));
              e;
            | None => continue(e)
            }
          | _ => continue(e)
          }
        },
      term,
    );
  result^;
};

/* Find the Match expression containing an arm whose pattern has the given ID. */
let find_match_containing_arm_by_pat =
    (target_pat_id: Id.t, term: Exp.t)
    : option((Id.t, Exp.t, list(case_arm), int)) => {
  let result = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Match(scrutinee, arms) =>
            switch (
              ListUtil.findi_opt(
                ((pat, _)) => Id.equal(Pat.rep_id(pat), target_pat_id),
                arms,
              )
            ) {
            | Some((idx, _)) =>
              result := Some((Exp.rep_id(e), scrutinee, arms, idx));
              e;
            | None => continue(e)
            }
          | _ => continue(e)
          }
        },
      term,
    );
  result^;
};

/* Find the Match expression containing an arm by either pattern or body ID. */
let find_match_containing_arm =
    (target_id: Id.t, term: Exp.t)
    : option((Id.t, Exp.t, list(case_arm), int)) =>
  switch (find_match_containing_arm_by_body(target_id, term)) {
  | Some(_) as result => result
  | None => find_match_containing_arm_by_pat(target_id, term)
  };

/* Parse a case arm string (e.g., "| Foo(x) => x + 1" or "Foo(x) => x + 1")
   by wrapping it in a dummy case expression and extracting the arm. */
let parse_case_arm = (code: string): option(case_arm) => {
  /* Strip leading | if present */
  let code =
    String.trim(code)
    |> (
      s =>
        if (String.length(s) > 0 && s.[0] == '|') {
          String.trim(String.sub(s, 1, String.length(s) - 1));
        } else {
          s;
        }
    );
  switch (parse_exp("case ? | " ++ code ++ " end")) {
  | Some(term) =>
    switch (Exp.term_of(term)) {
    | Match(_, [(pat, body)]) => Some((pat, body))
    | _ => None
    }
  | None => None
  };
};

/* Delete a case arm by index. */
let case_delete_arm =
    (z: Zipper.t, target_arm_body_id: Id.t): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_match_containing_arm(target_arm_body_id, term)) {
  | Some((match_id, _, arms, idx)) =>
    let new_arms = List.filteri((i, _) => i != idx, arms);
    let new_term = replace_match_arms(match_id, new_arms, term);
    Some(term_to_zipper(new_term));
  | None => None
  };
};

/* Insert a case arm before/after a reference arm. */
let case_insert_arm =
    (z: Zipper.t, target_arm_body_id: Id.t, code: string, d: Direction.t)
    : option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_match_containing_arm(target_arm_body_id, term)) {
  | Some((match_id, _, arms, idx)) =>
    switch (parse_case_arm(code)) {
    | Some(new_arm) =>
      let insert_at = d == Left ? idx : idx + 1;
      let (before, after) = ListUtil.split_n(insert_at, arms);
      let new_arms = before @ [new_arm] @ after;
      let new_term = replace_match_arms(match_id, new_arms, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

/* Update the body of a case arm.
   Copies secondary from old body to new to preserve positional whitespace. */
let case_update_arm_body =
    (z: Zipper.t, target_arm_body_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_match_containing_arm_by_body(target_arm_body_id, term)) {
  | Some((match_id, _, arms, idx)) =>
    switch (parse_exp(code)) {
    | Some(new_body) =>
      let new_arms =
        List.mapi(
          (i, (pat, body)) =>
            i == idx
              ? (pat, copy_exp_secondary(body, new_body)) : (pat, body),
          arms,
        );
      let new_term = replace_match_arms(match_id, new_arms, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

/* Update the pattern of a case arm.
   Copies secondary from old pattern to new to preserve positional whitespace. */
let case_update_arm_pattern =
    (z: Zipper.t, target_arm_body_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_match_containing_arm_by_body(target_arm_body_id, term)) {
  | Some((match_id, _, arms, idx)) =>
    switch (parse_pat(code)) {
    | Some(new_pat) =>
      let new_arms =
        List.mapi(
          (i, (pat, body)) =>
            i == idx
              ? (copy_pat_secondary(pat, new_pat), body) : (pat, body),
          arms,
        );
      let new_term = replace_match_arms(match_id, new_arms, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

/* Check if a target ID is inside a Match expression (is a case arm component) */
let is_case_arm = (z: Zipper.t, target_id: Id.t): bool => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_match_containing_arm(target_id, term)) {
  | Some(_) => true
  | None => false
  };
};

/* --- List element operations --- */

/* Replace the elements of a ListLit expression by ID. */
let replace_list_elements =
    (target_list_id: Id.t, new_elements: list(Exp.t), term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        if (Id.equal(Exp.rep_id(e), target_list_id)) {
          switch (Exp.term_of(e)) {
          | ListLit(_) => {
              ...e,
              term: ListLit(new_elements),
            }
          | _ => continue(e)
          };
        } else {
          continue(e);
        },
    term,
  );

let exp_contains_id = (root: Exp.t, target_id: Id.t): bool => {
  let found = ref(false);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          if (Id.equal(Exp.rep_id(e), target_id)) {
            found := true;
          };
          continue(e);
        },
      root,
    );
  found^;
};

/* Find the ListLit containing an element with the given ID.
   Returns (list_id, elements, element_index). */
let find_list_containing_element =
    (target_element_id: Id.t, term: Exp.t)
    : option((Id.t, list(Exp.t), int)) => {
  let result = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | ListLit(elements) =>
            switch (
              ListUtil.findi_opt(
                el =>
                  Id.equal(Exp.rep_id(el), target_element_id)
                  || exp_contains_id(el, target_element_id),
                elements,
              )
            ) {
            | Some((idx, _)) =>
              result := Some((Exp.rep_id(e), elements, idx));
              e;
            | None => continue(e)
            }
          | _ => continue(e)
          }
        },
      term,
    );
  result^;
};

let list_delete_element =
    (z: Zipper.t, target_element_id: Id.t): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_list_containing_element(target_element_id, term)) {
  | Some((list_id, elements, idx)) =>
    let new_elements = List.filteri((i, _) => i != idx, elements);
    let new_term = replace_list_elements(list_id, new_elements, term);
    Some(term_to_zipper(new_term));
  | None => None
  };
};

let list_insert_element =
    (z: Zipper.t, target_element_id: Id.t, code: string, d: Direction.t)
    : option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_list_containing_element(target_element_id, term)) {
  | Some((list_id, elements, idx)) =>
    switch (parse_exp(code)) {
    | Some(new_element) =>
      let insert_at = d == Left ? idx : idx + 1;
      let (before, after) = ListUtil.split_n(insert_at, elements);
      let new_elements = before @ [new_element] @ after;
      let new_term = replace_list_elements(list_id, new_elements, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

let list_update_element =
    (z: Zipper.t, target_element_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_list_containing_element(target_element_id, term)) {
  | Some((list_id, elements, idx)) =>
    switch (parse_exp(code)) {
    | Some(new_element) =>
      let new_elements =
        List.mapi((i, el) => i == idx ? new_element : el, elements);
      let new_term = replace_list_elements(list_id, new_elements, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

let is_list_element = (z: Zipper.t, target_id: Id.t): bool => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_list_containing_element(target_id, term)) {
  | Some(_) => true
  | None => false
  };
};

/* --- Tuple element operations --- */

/* Replace the elements of a Tuple expression by ID. */
let replace_tuple_elements =
    (target_tuple_id: Id.t, new_elements: list(Exp.t), term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        if (Id.equal(Exp.rep_id(e), target_tuple_id)) {
          switch (Exp.term_of(e)) {
          | Tuple(_) => {
              ...e,
              term: Tuple(new_elements),
            }
          | _ => continue(e)
          };
        } else {
          continue(e);
        },
    term,
  );

/* Find the Tuple containing an element with the given ID.
   Returns (tuple_id, elements, element_index). */
let find_tuple_containing_element =
    (target_element_id: Id.t, term: Exp.t)
    : option((Id.t, list(Exp.t), int)) => {
  let result = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Tuple(elements) =>
            switch (
              ListUtil.findi_opt(
                el =>
                  Id.equal(Exp.rep_id(el), target_element_id)
                  || exp_contains_id(el, target_element_id),
                elements,
              )
            ) {
            | Some((idx, _)) =>
              result := Some((Exp.rep_id(e), elements, idx));
              e;
            | None => continue(e)
            }
          | _ => continue(e)
          }
        },
      term,
    );
  result^;
};

let tuple_delete_element =
    (z: Zipper.t, target_element_id: Id.t): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_tuple_containing_element(target_element_id, term)) {
  | Some((tuple_id, elements, idx)) =>
    let new_elements = List.filteri((i, _) => i != idx, elements);
    let new_term = replace_tuple_elements(tuple_id, new_elements, term);
    Some(term_to_zipper(new_term));
  | None => None
  };
};

let tuple_insert_element =
    (z: Zipper.t, target_element_id: Id.t, code: string, d: Direction.t)
    : option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_tuple_containing_element(target_element_id, term)) {
  | Some((tuple_id, elements, idx)) =>
    switch (parse_exp(code)) {
    | Some(new_element) =>
      let insert_at = d == Left ? idx : idx + 1;
      let (before, after) = ListUtil.split_n(insert_at, elements);
      let new_elements = before @ [new_element] @ after;
      let new_term = replace_tuple_elements(tuple_id, new_elements, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

let tuple_update_element =
    (z: Zipper.t, target_element_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_tuple_containing_element(target_element_id, term)) {
  | Some((tuple_id, elements, idx)) =>
    switch (parse_exp(code)) {
    | Some(new_element) =>
      let new_elements =
        List.mapi((i, el) => i == idx ? new_element : el, elements);
      let new_term = replace_tuple_elements(tuple_id, new_elements, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

let is_tuple_element = (z: Zipper.t, target_id: Id.t): bool => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (find_tuple_containing_element(target_id, term)) {
  | Some(_) => true
  | None => false
  };
};

/* Check if a target ID is any kind of sequence element (case arm, list, or tuple) */
let is_sequence_element = (z: Zipper.t, target_id: Id.t): bool =>
  is_case_arm(z, target_id)
  || is_list_element(z, target_id)
  || is_tuple_element(z, target_id);

/* --- Insert dispatch (shared by path-based and selector-based insert) --- */

/* Find the Let/TyAlias/ModuleExp node for a binding anchor.
   Accepts the binding node itself or any sub-id (pattern, definition, etc.). */
let find_let_binding_id = (z: Zipper.t, exp_id: Id.t): option(Id.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let result = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          if (Id.equal(Exp.rep_id(e), exp_id)) {
            switch (Exp.term_of(e)) {
            | Let(_)
            | TyAlias(_)
            | ModuleExp(_) => result := Some(Exp.rep_id(e))
            | _ => ()
            };
          };
          switch (Exp.term_of(e)) {
          | Let(pat, def, _) =>
            if (Id.equal(Pat.rep_id(pat), exp_id)
                || Id.equal(Exp.rep_id(def), exp_id)) {
              result := Some(Exp.rep_id(e));
            }
          | TyAlias(tpat, tdef, _) =>
            if (Id.equal(TPat.rep_id(tpat), exp_id)
                || Id.equal(Typ.rep_id(tdef), exp_id)) {
              result := Some(Exp.rep_id(e));
            }
          | ModuleExp(mpat, def, _) =>
            if (Id.equal(IdTagged.rep_id(mpat), exp_id)
                || Id.equal(Exp.rep_id(def), exp_id)) {
              result := Some(Exp.rep_id(e));
            }
          | _ => ()
          };
          continue(e);
        },
      term,
    );
  result^;
};

/* Whether target is a direct child of a semicolon sequence (test line, etc.). */
let find_seq_containing_child =
    (target_id: Id.t, term: Exp.t): option((Id.t, Direction.t)) => {
  let found = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Seq(e1, e2) =>
            if (Id.equal(Exp.rep_id(e1), target_id)) {
              found := Some((Exp.rep_id(e), Direction.Left));
            } else if (Id.equal(Exp.rep_id(e2), target_id)) {
              found := Some((Exp.rep_id(e), Direction.Right));
            };
            continue(e);
          | _ => continue(e)
          }
        },
      term,
    );
  found^;
};

/* If target is inside a semicolon-separated line, return that line's root id. */
let normalize_seq_line_anchor = (target_id: Id.t, term: Exp.t): option(Id.t) => {
  switch (find_seq_containing_child(target_id, term)) {
  | Some(_) => Some(target_id)
  | None =>
    let line_id = ref(None);
    let _ =
      Exp.map_term(
        ~f_exp=
          (continue, e) => {
            switch (Exp.term_of(e)) {
            | Seq(e1, e2) =>
              if (Id.equal(Exp.rep_id(e1), target_id)
                  || exp_contains_id(e1, target_id)) {
                line_id := Some(Exp.rep_id(e1));
              } else if (Id.equal(Exp.rep_id(e2), target_id)
                         || exp_contains_id(e2, target_id)) {
                line_id := Some(Exp.rep_id(e2));
              };
              continue(e);
            | _ => continue(e)
            }
          },
        term,
      );
    line_id^;
  };
};

/* Insert before/after a direct child of a semicolon-separated sequence. */
let seq_insert_sibling =
    (z: Zipper.t, target_id: Id.t, code: string, d: Direction.t)
    : option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let target_id =
    switch (normalize_seq_line_anchor(target_id, term)) {
    | Some(id) => id
    | None => target_id
    };
  switch (find_seq_containing_child(target_id, term)) {
  | None => None
  | Some((seq_id, side)) =>
    switch (parse_exp(code)) {
    | None => None
    | Some(new_line) =>
      let new_line =
        ensure_leading_secondary(~default=mk_newline(), new_line);
      let new_term =
        replace_exp_by_id(
          seq_id,
          parent => {
            switch (Exp.term_of(parent), side, d) {
            | (Seq(e1, e2), Left, Left) => {
                ...parent,
                term:
                  Seq(
                    new_line,
                    Exp.fresh(Seq(ensure_leading_secondary(e1), e2)),
                  ),
              }
            | (Seq(e1, e2), Left, Right) => {
                ...parent,
                term:
                  Seq(
                    e1,
                    Exp.fresh(Seq(new_line, ensure_leading_secondary(e2))),
                  ),
              }
            | (Seq(e1, e2), Right, Left) => {
                ...parent,
                term:
                  Seq(
                    e1,
                    Exp.fresh(Seq(new_line, ensure_leading_secondary(e2))),
                  ),
              }
            | (Seq(e1, e2), Right, Right) => {
                ...parent,
                term: Seq(e1, Exp.fresh(Seq(e2, new_line))),
              }
            | _ => parent
            }
          },
          term,
        );
      Some(term_to_zipper(new_term));
    }
  };
};

/* --- Update operations on let-chain bindings --- */

/* Update the definition of a binding (the expression after = ).
   target_id: the ID of the definition expression to replace. */
let update_definition =
    (z: Zipper.t, target_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (parse_exp(code)) {
  | Some(new_def) =>
    let new_term = replace_exp_by_id(target_id, _ => new_def, term);
    Some(term_to_zipper(new_term));
  | None => None
  };
};

/* Update the body of a binding (the expression after in).
   target_id: the ID of the body expression to replace. */
let update_body =
    (z: Zipper.t, target_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (parse_exp(code)) {
  | Some(new_body) =>
    let new_term = replace_exp_by_id(target_id, _ => new_body, term);
    Some(term_to_zipper(new_term));
  | None => None
  };
};

/* Check if a pattern binds a given variable name. */
let rec pat_binds = (pat: Pat.t, name: string): bool =>
  switch (Pat.term_of(pat)) {
  | Var(n) => String.equal(n, name)
  | Asc(p, _)
  | Parens(p) => pat_binds(p, name)
  | Tuple(ps) => List.exists(p => pat_binds(p, name), ps)
  | Cons(p1, p2) => pat_binds(p1, name) || pat_binds(p2, name)
  | _ => false
  };

/* Get the variable name from a pattern (simple or annotated). */
let pat_var_name = (pat: Pat.t): option(string) =>
  switch (Pat.term_of(pat)) {
  | Var(n) => Some(n)
  | Asc(p, _) =>
    switch (Pat.term_of(p)) {
    | Var(n) => Some(n)
    | _ => None
    }
  | _ => None
  };

/* Rename a variable in an expression tree, respecting shadowing.
   Stops renaming in the body of any Let/Fun that re-binds the same name. */
let rename_var_in_exp =
    (old_name: string, new_name: string, term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        switch (Exp.term_of(e)) {
        | Var(name) when String.equal(name, old_name) => {
            ...e,
            term: Var(new_name),
          }
        | Let(pat, def, body) when pat_binds(pat, old_name) =>
          /* Shadowed: rename in def (for recursive bindings) but not body */
          {
            ...e,
            term: Let(pat, continue(def), body),
          }
        | _ => continue(e)
        },
    term,
  );

/* Update a pattern in a binding and rename all variable references.
   target_id: the ID of the pattern to replace.
   Does the rename at the term level (before round-trip) so it works
   correctly regardless of ID changes from the round-trip. */
let update_pattern =
    (z: Zipper.t, target_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (parse_pat(code)) {
  | Some(new_pat) =>
    let new_name = pat_var_name(new_pat);
    /* Find the Let or ModLet containing this pattern, replace it, and rename vars */
    let new_term =
      Exp.map_term(
        ~f_exp=
          (continue, e) =>
            switch (Exp.term_of(e)) {
            | Let(pat, def, body) when Id.equal(Pat.rep_id(pat), target_id) =>
              let old_name = pat_var_name(pat);
              /* Rename variables in def and body */
              let (def, body) =
                switch (old_name, new_name) {
                | (Some(old_n), Some(new_n))
                    when !String.equal(old_n, new_n) => (
                    rename_var_in_exp(old_n, new_n, def),
                    rename_var_in_exp(old_n, new_n, body),
                  )
                | _ => (def, body)
                };
              {
                ...e,
                term: Let(new_pat, continue(def), continue(body)),
              };
            | Module(items) =>
              /* Check if any ModLet has the target pattern */
              let target_idx =
                ListUtil.findi_opt(
                  (item: Mod.t) =>
                    switch (item.term) {
                    | ModLet(pat, _) => Id.equal(Pat.rep_id(pat), target_id)
                    | _ => false
                    },
                  items,
                );
              switch (target_idx) {
              | Some((idx, target_item)) =>
                let old_name =
                  switch (target_item.term) {
                  | ModLet(pat, _) => pat_var_name(pat)
                  | _ => None
                  };
                let should_rename =
                  switch (old_name, new_name) {
                  | (Some(old_n), Some(new_n))
                      when !String.equal(old_n, new_n) =>
                    Some((old_n, new_n))
                  | _ => None
                  };
                let new_items =
                  List.mapi(
                    (i, item: Mod.t) =>
                      if (i == idx) {
                        switch (item.term) {
                        | ModLet(_, def) =>
                          let def =
                            switch (should_rename) {
                            | Some((old_n, new_n)) =>
                              rename_var_in_exp(old_n, new_n, def)
                            | None => def
                            };
                          let new_term: TermBase.mod_term =
                            ModLet(new_pat, def);
                          {
                            ...item,
                            term: new_term,
                          };
                        | _ => item
                        };
                      } else if (i > idx) {
                        switch (should_rename) {
                        | Some((old_n, new_n)) =>
                          switch (item.term) {
                          | ModLet(p, def) =>
                            let new_term: TermBase.mod_term =
                              ModLet(
                                p,
                                rename_var_in_exp(old_n, new_n, def),
                              );
                            {
                              ...item,
                              term: new_term,
                            };
                          | ModExp(exp) =>
                            let new_term: TermBase.mod_term =
                              ModExp(rename_var_in_exp(old_n, new_n, exp));
                            {
                              ...item,
                              term: new_term,
                            };
                          | _ => item
                          }
                        | None => item
                        };
                      } else {
                        item;
                      },
                    items,
                  );
                {
                  ...e,
                  term: Module(new_items),
                };
              | None => continue(e)
              };
            | _ => continue(e)
            },
        term,
      );
    Some(term_to_zipper(new_term));
  | None => None
  };
};

/* Update a type annotation.
   target_id: the ID of the type to replace. */
let update_type_annotation =
    (z: Zipper.t, target_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (parse_typ(code)) {
  | Some(new_typ) =>
    let new_term = replace_typ_by_id(target_id, new_typ, term);
    Some(term_to_zipper(new_term));
  | None => None
  };
};

/* Replace the innermost body of a Let/TyAlias chain with the given body.
   For `Let(a, 1, Let(x, a+10, _))`, replaces the `_` with `body`. */
let rec replace_innermost_body = (parsed: Exp.t, body: Exp.t): Exp.t =>
  switch (Exp.term_of(parsed)) {
  | Let(p, d, inner) => {
      ...parsed,
      term: Let(p, d, replace_innermost_body(inner, body)),
    }
  | TyAlias(tp, td, inner) => {
      ...parsed,
      term: TyAlias(tp, td, replace_innermost_body(inner, body)),
    }
  | _ => body
  };

/* Update the entire binding clause of a let expression.
   Parses the code and replaces the Let/TyAlias node's pat+def,
   threading the original body as the innermost body of the replacement chain.
   target_id: the ID of the Let/TyAlias expression itself. */
let update_binding_clause =
    (z: Zipper.t, target_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  switch (parse_exp(code)) {
  | Some(parsed) =>
    let new_term =
      replace_exp_by_id(
        target_id,
        e =>
          switch (Exp.term_of(e), Exp.term_of(parsed)) {
          /* Replace binding clause(s), keeping the original body */
          | (Let(_, _, body), Let(_, _, _)) =>
            replace_innermost_body(parsed, body)
          /* Replace tpat+tdef of a TyAlias, keeping the original body */
          | (TyAlias(_, _, body), TyAlias(_, _, _)) =>
            replace_innermost_body(parsed, body)
          /* For bare expressions (Seq items), replace entirely */
          | _ => parsed
          },
        term,
      );
    Some(term_to_zipper(new_term));
  | None => None
  };
};

/* Delete a binding from a let-chain.
   For Let(pat, def, body): removes the binding and replaces with body.
   For Seq(e, rest): removes e and replaces with rest.
   target_id: the ID of the Let/TyAlias/Seq/ModuleExp expression. */
let delete_binding = (z: Zipper.t, target_id: Id.t): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let new_term =
    replace_exp_by_id(
      target_id,
      e =>
        switch (Exp.term_of(e)) {
        | Let(_, _, body) => body
        | TyAlias(_, _, body) => body
        | ModuleExp(_, _, body) => body
        | Seq(_, rest) => rest
        | _ => e /* Can't delete a non-binding expression */
        },
      term,
    );
  Some(term_to_zipper(new_term));
};

/* Delete a body expression.
   Replaces the body with an empty hole.
   target_id: the ID of the body expression. */
let delete_body = (z: Zipper.t, target_id: Id.t): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let hole = Exp.fresh(EmptyHole);
  let new_term = replace_exp_by_id(target_id, _ => hole, term);
  Some(term_to_zipper(new_term));
};

/* Insert a new binding before or after a target binding.
   For let-chains: wraps the target or its body in a new Let.
   target_id: the ID of the reference Let/TyAlias expression.
   code: the text to parse as a new binding (e.g. "let z = 3").
   d: Left = before target, Right = after target. */
let insert_binding =
    (z: Zipper.t, target_id: Id.t, code: string, d: Direction.t)
    : option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  /* Strip trailing " in" from code, since we add our own " in 0" */
  let code = {
    let trimmed = String.trim(code);
    if (String.ends_with(trimmed, ~suffix=" in")) {
      String.sub(trimmed, 0, String.length(trimmed) - 3);
    } else {
      trimmed;
    };
  };
  switch (parse_exp(code ++ " in 0")) {
  | Some(parsed) =>
    switch (Exp.term_of(parsed)) {
    | Let(new_pat, new_def, _) =>
      let new_term =
        replace_exp_by_id(
          target_id,
          e =>
            switch (d) {
            | Left =>
              /* Insert before: wrap target in new Let.
                 Ensure body has leading whitespace after "in". */
              fresh_exp_with_secondary(
                Let(new_pat, new_def, ensure_leading_secondary(e)),
              )
            | Right =>
              /* Insert after: wrap target's body in new Let.
                 The new Let gets a newline before it. */
              switch (Exp.term_of(e)) {
              | Let(pat, def, body) => {
                  ...e,
                  term:
                    Let(
                      pat,
                      def,
                      fresh_exp_with_secondary(
                        ~before=[mk_newline()],
                        Let(new_pat, new_def, body),
                      ),
                    ),
                }
              | TyAlias(tpat, tdef, body) => {
                  ...e,
                  term:
                    TyAlias(
                      tpat,
                      tdef,
                      fresh_exp_with_secondary(
                        ~before=[mk_newline()],
                        Let(new_pat, new_def, body),
                      ),
                    ),
                }
              | _ =>
                /* For bare expressions, wrap in a Seq */
                Exp.fresh(
                  Seq(
                    e,
                    fresh_exp_with_secondary(
                      ~before=[mk_newline()],
                      Let(
                        new_pat,
                        new_def,
                        fresh_exp_with_secondary(
                          ~before=[mk_space()],
                          EmptyHole,
                        ),
                      ),
                    ),
                  ),
                )
              }
            },
          term,
        );
      Some(term_to_zipper(new_term));
    | TyAlias(new_tpat, new_tdef, _) =>
      let new_term =
        replace_exp_by_id(
          target_id,
          e =>
            switch (d) {
            | Left =>
              fresh_exp_with_secondary(
                TyAlias(new_tpat, new_tdef, ensure_leading_secondary(e)),
              )
            | Right =>
              switch (Exp.term_of(e)) {
              | Let(pat, def, body) => {
                  ...e,
                  term:
                    Let(
                      pat,
                      def,
                      fresh_exp_with_secondary(
                        ~before=[mk_newline()],
                        TyAlias(new_tpat, new_tdef, body),
                      ),
                    ),
                }
              | TyAlias(tpat, tdef, body) => {
                  ...e,
                  term:
                    TyAlias(
                      tpat,
                      tdef,
                      fresh_exp_with_secondary(
                        ~before=[mk_newline()],
                        TyAlias(new_tpat, new_tdef, body),
                      ),
                    ),
                }
              | _ =>
                Exp.fresh(
                  Seq(
                    e,
                    fresh_exp_with_secondary(
                      ~before=[mk_newline()],
                      TyAlias(
                        new_tpat,
                        new_tdef,
                        fresh_exp_with_secondary(
                          ~before=[mk_space()],
                          EmptyHole,
                        ),
                      ),
                    ),
                  ),
                )
              }
            },
          term,
        );
      Some(term_to_zipper(new_term));
    | _ => None /* Couldn't parse as a binding */
    }
  | None => None
  };
};

/* Label for insert failure messages (matches the dispatch branch attempted). */
let insert_kind_label = (z: Zipper.t, target_id: Id.t): string =>
  if (is_case_arm(z, target_id)) {
    "case arm";
  } else if (is_list_element(z, target_id)) {
    "list element";
  } else if (is_tuple_element(z, target_id)) {
    "tuple element";
  } else if (is_module_item(z, target_id)
             || find_module_item_id(z, target_id) != None) {
    "module item";
  } else if (find_let_binding_id(z, target_id) != None) {
    "binding";
  } else {
    switch (
      normalize_seq_line_anchor(
        target_id,
        MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
      )
    ) {
    | Some(_) => "sequence line"
    | None => "binding"
    };
  };

/* Unified insert: case/list/tuple/module/binding/seq, with binding fallback. */
let try_insert_at =
    (z: Zipper.t, target_id: Id.t, code: string, d: Direction.t)
    : option((Zipper.t, string)) => {
  let finish = (label, result) =>
    switch (result) {
    | Some(new_z) => Some((new_z, label))
    | None => None
    };
  if (is_case_arm(z, target_id)) {
    finish("case arm", case_insert_arm(z, target_id, code, d));
  } else if (is_list_element(z, target_id)) {
    finish("list element", list_insert_element(z, target_id, code, d));
  } else if (is_tuple_element(z, target_id)) {
    finish("tuple element", tuple_insert_element(z, target_id, code, d));
  } else {
    switch (find_module_item_id(z, target_id)) {
    | Some(item_id) =>
      finish("module item", module_insert(z, item_id, code, d))
    | None =>
      switch (find_let_binding_id(z, target_id)) {
      | Some(let_id) =>
        finish("binding", insert_binding(z, let_id, code, d))
      | None =>
        switch (seq_insert_sibling(z, target_id, code, d)) {
        | Some(new_z) => Some((new_z, "sequence line"))
        | None => finish("binding", insert_binding(z, target_id, code, d))
        }
      }
    };
  };
};
