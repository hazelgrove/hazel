/* TermEdit: Term-level syntax transformations for structural editing.

      Instead of manipulating the zipper/segment directly (which has sort-context
      issues for module items), we:
      1. Get the full program term from the zipper
      2. Modify the term tree (splice in/out sub-terms)
      3. Convert back to a segment via ExpToSegment with AutoFormat
      4. Create a new zipper from the modified segment

      This approach is sort-correct by construction and handles modules cleanly.
      AutoFormat uses should_add_space heuristics for whitespace, which produces
      correct spacing for programmatic edits without relying on stored secondary.
   */

open Util;
open Language;

/* Round-trip settings: use AutoFormat for heuristic spacing.
   This produces consistent, correct whitespace for programmatic edits.
   AutoFormat adds spaces based on should_add_space heuristics in mk_form,
   rather than relying on stored secondary (which may be empty on freshly
   created terms). */
let roundtrip_settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
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
  switch (Parser.to_term(code)) {
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
   Left = before idx, Right = after idx. */
let insert_item =
    (items: list(Mod.t), idx: int, new_item: Mod.t, d: Direction.t)
    : list(Mod.t) => {
  let insert_at = d == Left ? idx : idx + 1;
  let (before, after) = ListUtil.split_n(insert_at, items);
  before @ [new_item] @ after;
};

/* Replace a module item at a position in the items list. */
let replace_item =
    (items: list(Mod.t), idx: int, new_item: Mod.t): list(Mod.t) =>
  List.mapi((i, item) => i == idx ? new_item : item, items);

/* --- High-level edit operations --- */

/* Delete a module item cleanly (no hole left).
   target_item_id: the ID of the ModLet/ModType item to delete.
   Returns the modified zipper, or None if the item wasn't found. */
let module_delete = (z: Zipper.t, target_item_id: Id.t): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
  switch (find_module_containing_item(target_id, term)) {
  | Some(_) => true
  | None => false
  };
};

/* --- General-purpose term-level edit operations --- */

/* Replace a sub-expression by ID anywhere in the term tree.
   The replacement function receives the matched expression and returns
   the new expression to substitute. */
let replace_exp_by_id =
    (target_id: Id.t, f: Exp.t => Exp.t, term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        if (Id.equal(Exp.rep_id(e), target_id)) {
          f(e);
        } else {
          continue(e);
        },
    term,
  );

/* Replace a sub-pattern by ID. Walks the term tree and within any
   expression that contains the target pattern, replaces it. */
let replace_pat_by_id = (target_id: Id.t, new_pat: Pat.t, term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_pat=
      (continue, p) =>
        if (Id.equal(Pat.rep_id(p), target_id)) {
          new_pat;
        } else {
          continue(p);
        },
    term,
  );

/* Replace a sub-type by ID. */
let replace_typ_by_id = (target_id: Id.t, new_typ: Typ.t, term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_typ=
      (continue, t) =>
        if (Id.equal(Typ.rep_id(t), target_id)) {
          new_typ;
        } else {
          continue(t);
        },
    term,
  );

/* Replace a sub-tpat by ID. */
let replace_tpat_by_id =
    (target_id: Id.t, new_tpat: TPat.t, term: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_tpat=
      (continue, tp) =>
        if (Id.equal(TPat.rep_id(tp), target_id)) {
          new_tpat;
        } else {
          continue(tp);
        },
    term,
  );

/* Parse code as an expression term.
   Returns None if the code has parse errors (unmatched delimiters,
   invalid tokens, or malformed expressions). */
let parse_exp = (code: string): option(Exp.t) =>
  switch (Parser.to_zipper(code)) {
  | Some(z) =>
    /* Check for unmatched delimiters in backpack */
    let backpack = Zipper.local_backpack(z);
    switch (backpack) {
    | [_, ..._] => None
    | [] =>
      let term = MakeTerm.from_zip_for_sem(z).term;
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
  switch (Parser.to_term("let " ++ code ++ " = 0 in 0")) {
  | Some(term) =>
    switch (Exp.term_of(term)) {
    | Let(pat, _, _) => Some(pat)
    | _ => None
    }
  | None => None
  };

/* Parse code as a type term */
let parse_typ = (code: string): option(Typ.t) =>
  switch (Parser.to_term("let x : " ++ code ++ " = 0 in 0")) {
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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

/* Update the body of a case arm. */
let case_update_arm_body =
    (z: Zipper.t, target_arm_body_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z).term;
  switch (find_match_containing_arm_by_body(target_arm_body_id, term)) {
  | Some((match_id, _, arms, idx)) =>
    switch (parse_exp(code)) {
    | Some(new_body) =>
      let new_arms =
        List.mapi(
          (i, (pat, body)) => i == idx ? (pat, new_body) : (pat, body),
          arms,
        );
      let new_term = replace_match_arms(match_id, new_arms, term);
      Some(term_to_zipper(new_term));
    | None => None
    }
  | None => None
  };
};

/* Update the pattern of a case arm. */
let case_update_arm_pattern =
    (z: Zipper.t, target_arm_body_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z).term;
  switch (find_match_containing_arm_by_body(target_arm_body_id, term)) {
  | Some((match_id, _, arms, idx)) =>
    switch (parse_pat(code)) {
    | Some(new_pat) =>
      let new_arms =
        List.mapi(
          (i, (pat, body)) => i == idx ? (new_pat, body) : (pat, body),
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
                el => Id.equal(Exp.rep_id(el), target_element_id),
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
                el => Id.equal(Exp.rep_id(el), target_element_id),
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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

/* --- Update operations on let-chain bindings --- */

/* Update the definition of a binding (the expression after = ).
   target_id: the ID of the definition expression to replace. */
let update_definition =
    (z: Zipper.t, target_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
  switch (parse_typ(code)) {
  | Some(new_typ) =>
    let new_term = replace_typ_by_id(target_id, new_typ, term);
    Some(term_to_zipper(new_term));
  | None => None
  };
};

/* Update the entire binding clause of a let expression.
   Parses the code and replaces the Let/TyAlias node's pat+def.
   target_id: the ID of the Let/TyAlias expression itself. */
let update_binding_clause =
    (z: Zipper.t, target_id: Id.t, code: string): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z).term;
  switch (parse_exp(code)) {
  | Some(parsed) =>
    let new_term =
      replace_exp_by_id(
        target_id,
        e =>
          switch (Exp.term_of(e), Exp.term_of(parsed)) {
          /* Replace pat+def of a Let, keeping the original body */
          | (Let(_, _, body), Let(new_pat, new_def, _)) => {
              ...e,
              term: Let(new_pat, new_def, body),
            }
          /* Replace tpat+tdef of a TyAlias, keeping the original body */
          | (TyAlias(_, _, body), TyAlias(new_tpat, new_tdef, _)) => {
              ...e,
              term: TyAlias(new_tpat, new_tdef, body),
            }
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
   target_id: the ID of the Let/TyAlias/Seq expression. */
let delete_binding = (z: Zipper.t, target_id: Id.t): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z).term;
  let new_term =
    replace_exp_by_id(
      target_id,
      e =>
        switch (Exp.term_of(e)) {
        | Let(_, _, body) => body
        | TyAlias(_, _, body) => body
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
  let term = MakeTerm.from_zip_for_sem(z).term;
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
              /* Insert before: wrap target in new Let */
              Exp.fresh(Let(new_pat, new_def, e))
            | Right =>
              /* Insert after: wrap target's body in new Let */
              switch (Exp.term_of(e)) {
              | Let(pat, def, body) => {
                  ...e,
                  term:
                    Let(pat, def, Exp.fresh(Let(new_pat, new_def, body))),
                }
              | TyAlias(tpat, tdef, body) => {
                  ...e,
                  term:
                    TyAlias(
                      tpat,
                      tdef,
                      Exp.fresh(Let(new_pat, new_def, body)),
                    ),
                }
              | _ =>
                /* For bare expressions, wrap in a Seq */
                Exp.fresh(
                  Seq(
                    e,
                    Exp.fresh(Let(new_pat, new_def, Exp.fresh(EmptyHole))),
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
            | Left => Exp.fresh(TyAlias(new_tpat, new_tdef, e))
            | Right =>
              switch (Exp.term_of(e)) {
              | Let(pat, def, body) => {
                  ...e,
                  term:
                    Let(
                      pat,
                      def,
                      Exp.fresh(TyAlias(new_tpat, new_tdef, body)),
                    ),
                }
              | TyAlias(tpat, tdef, body) => {
                  ...e,
                  term:
                    TyAlias(
                      tpat,
                      tdef,
                      Exp.fresh(TyAlias(new_tpat, new_tdef, body)),
                    ),
                }
              | _ =>
                Exp.fresh(
                  Seq(
                    e,
                    Exp.fresh(
                      TyAlias(new_tpat, new_tdef, Exp.fresh(EmptyHole)),
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
