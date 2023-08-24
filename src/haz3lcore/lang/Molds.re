open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type expansions = list((Token.t, (list(Token.t), Direction.t)));

let forms_assoc: list((Label.t, list(Mold.t))) =
  List.fold_left(
    (acc, (_, {label, mold, _}: Form.t)) => {
      let molds =
        switch (List.assoc_opt(label, acc)) {
        | Some(old_molds) => old_molds @ [mold]
        | None => [mold]
        };
      List.cons((label, molds), List.remove_assoc(label, acc));
    },
    [],
    Form.forms,
  );

let get = (label: Label.t): list(Mold.t) =>
  switch (label, List.assoc_opt(label, forms_assoc)) {
  | ([t], Some(molds)) when Form.atomic_molds(t) != [] =>
    // TODO(andrew): does this make sense?
    Form.atomic_molds(t) @ molds
  | ([t], None) when Form.atomic_molds(t) != [] => Form.atomic_molds(t)
  | (_, Some(molds)) => molds
  | ([t], None) =>
    /* For tokens which are not assigned molds by the language definition,
       assing a default 'Any' mold, which is either convex or concave
       depending on the first character. This is a heuristic at the
       moment as we don't currently rigorously divide token classes
       for operators vs operands, but is somewhat load-bearing in that
       remolding as one is typing in a multi-character operator can cause
       jank, which is alleviated if we correctly guess that it will
       become an operator. Alternatively, this could be based on
       logic which checks if the token is the prefix of whatever. */
    switch (Form.is_potential_operand(t), Form.is_potential_operator(t)) {
    | (true, false) => [Mold.mk_op(Any, [])]
    | (false, true) => [Mold.mk_bin(Precedence.max, Any, [])]
    | (true, true) =>
      Printf.printf(
        "Warning: Molds.get: can't decide if operand or operator: '%s'\n",
        t,
      );
      [Mold.mk_op(Any, [])];
    | (false, false) =>
      //TODO(andrew): this is triggered in all instant expand cases ([]()|, etc)
      //Printf.printf("Warning: Molds.get: unhandled mono: '%s'\n", t);
      [Mold.mk_op(Any, [])]
    }
  | (lbl, None) =>
    Printf.printf(
      "Warning: Molds.get: unhandled label: '%s'\n",
      String.concat(" ", lbl),
    );
    [Mold.mk_op(Any, [])];
  };

let delayed_expansions: expansions =
  List.filter_map(
    ((_, {expansion, label, _}: Form.t)) =>
      switch (expansion, label) {
      | ((Delayed, Delayed), [hd, ..._]) =>
        Some([
          (hd, (label, Direction.Left)),
          (ListUtil.last(label), (label, Right)),
        ])
      | ((Delayed, _), [hd, ..._]) => Some([(hd, (label, Left))])
      | ((_, Delayed), [_, ..._]) =>
        Some([(ListUtil.last(label), (label, Right))])
      | _ => None
      },
    Form.forms,
  )
  |> List.flatten
  |> List.sort_uniq(compare);

let instant_expansions: expansions =
  List.filter_map(
    ((_, {expansion, label, _}: Form.t)) =>
      switch (expansion, label) {
      | ((Instant, Instant), [hd, ..._]) =>
        Some([
          (hd, (label, Direction.Left)),
          (ListUtil.last(label), (label, Right)),
        ])
      | ((Instant, _), [hd, ..._]) => Some([(hd, (label, Left))])
      | ((_, Instant), [_, ..._]) =>
        Some([(ListUtil.last(label), (label, Right))])
      | _ => None
      },
    Form.forms,
  )
  |> List.flatten
  |> List.sort_uniq(compare);

let delayed_expansion: Token.t => (list(Token.t), Direction.t) =
  s =>
    /* Completions which must be defered as they are ambiguous prefixes */
    switch (List.assoc_opt(s, delayed_expansions)) {
    | Some(expansion) => expansion
    | None => ([s], Right)
    };

let instant_expansion: Token.t => (list(Token.t), Direction.t) =
  s =>
    /* Completions which can or must be executed immediately */
    switch (List.assoc_opt(s, instant_expansions)) {
    | Some(expansion) => expansion
    | None => ([s], Right)
    };

let is_delayed = kw => List.length(delayed_expansion(kw) |> fst) > 1;

/*

 TODO(andrew): cleanup

 Maybe more advanced token allow logic:

 Goal: Try to preserve the prefix-validity of tokens as much as possible
 when inserting/deleting characters, but allow the creation of prefix-
 invalid tokens as a fallthrough. Always be willing to split on insert
 to maintain prefix-validity, but allow the creation of prefix-invalid
 tokens for deletes which would otherwise be no-ops.

 Or: if insert/delete results in a thing which is no longer a prefix
 of a valid token, then we split it into the largest prefix of
 a valid token and up to two more tokens; if there are two, one is
 a single character (doesn't quite work for delete; we can't guarantee
 the existence of such a split eg if lang consists of single "yo", deleting
 the "y" leaves no possible prefixes)

 for this to be nice if we could establish that the split
 must always occur at the caret; otherwise it might be confusing.

 precondition: caret is either inside a token, or between two tokens
 any tokens are prefix-valid

 insert between case: attempt to append to left, then the right, otherwise new
 (so everything always prefix-valid)

 insert inside case: attempt to insert. otherwise go to between case
 (so everything always prefix-valid)
 delete from right: can't effect prefix-validity
 delete from left/inside: ??? allow create prefix-invalid ???
 delete-merge (when delete an intervening seg, pushing two tokens together):
   if the result is a valid token, great; otherwise ??? allow create prefix-invalid ???


 */

//let is_instant = kw => List.length(instant_expansion(kw) |> fst) > 1;

//let append_safe = char =>
//  !is_instant(char)
//  && !Form.is_secondary(char)
//  && !(Form.is_string_delim(char) || Form.is_comment_delim(char));

let allow_merge = (l: Token.t, r: Token.t): bool =>
  Form.is_potential_token(l ++ r);
//  Form.is_valid_token(l ++ r)
//  || !(Form.is_valid_token(l) && Form.is_valid_token(r));
// alternatively, require l++r is valid (simpler, more restrictiive)

let allow_append_right = (t: Token.t, char: string): bool =>
  Form.is_potential_token(t ++ char); //&& append_safe(char);
/*
 Form.is_valid_token(t ++ char)
 || (
   !Form.is_valid_token(t ++ char)
   && !(Form.is_valid_token(t) && Form.is_valid_token(char))
 ) && append_safe(char)*/

let allow_append_left = (char: string, t: Token.t): bool =>
  Form.is_potential_token(t ++ char); // && append_safe(char);
//  Form.is_valid_token(char ++ t)
//  || (!Form.is_valid_token(t) && !Form.is_valid_token(char)) && append_safe(char)

let allow_insertion = (_char: string, _t: Token.t, new_t: Token.t): bool =>
  Form.is_potential_token(new_t); // && append_safe(char);
//  Form.is_valid_token(new_t) || !Form.is_valid_token(t) && append_safe(char);

let delayed_leading_delims = (sort: Sort.t): list(Token.t) =>
  Form.delims
  |> List.map(token => {
       let (lbl, _) = delayed_expansion(token);
       List.filter_map(
         (m: Mold.t) =>
           List.length(lbl) > 1 && token == List.hd(lbl) && m.out == sort
             ? Some(token ++ " ") : None,
         get(lbl),
       );
     })
  |> List.flatten
  |> List.sort_uniq(compare);
let delated_leading_delims_exp = delayed_leading_delims(Exp);
let delated_leading_delims_pat = delayed_leading_delims(Pat);
let delated_leading_delims_typ = delayed_leading_delims(Typ);
let delayed_leading_delims = (sort: Sort.t): list(string) =>
  switch (sort) {
  | Exp => delated_leading_delims_exp
  | Pat => delated_leading_delims_pat
  | Typ => delated_leading_delims_typ
  | _ => []
  };

let infix_delims = (sort: Sort.t): list(Token.t) =>
  Form.delims
  |> List.map(token => {
       List.filter_map(
         (m: Mold.t) =>
           m.out == sort && Mold.is_infix_op(m) ? Some(token) : None,
         get([token]),
       )
     })
  |> List.flatten
  |> List.sort_uniq(compare);
let infix_delims_exp = infix_delims(Exp);
let infix_delims_pat = infix_delims(Pat);
let infix_delims_typ = infix_delims(Typ);
let infix_delims = (sort: Sort.t): list(string) =>
  switch (sort) {
  | Exp => infix_delims_exp
  | Pat => infix_delims_pat
  | Typ => infix_delims_typ
  | _ => []
  };

let const_mono_delims = (sort: Sort.t): list(Token.t) =>
  Form.const_mono_delims  //TODO(andrew): refactor to incorporate with Form.delims?
  |> List.map(token => {
       List.filter_map(
         (m: Mold.t) =>
           m.out == sort && List.mem(token, Form.const_mono_delims)
             ? Some(token) : None,
         get([token]),
       )
     })
  |> List.flatten
  |> List.sort_uniq(compare);
let const_mono_delims_exp = const_mono_delims(Exp);
let const_mono_delims_pat = const_mono_delims(Pat);
let const_mono_delims_typ = const_mono_delims(Typ);
let const_mono_delims = (sort: Sort.t): list(string) =>
  switch (sort) {
  | Exp => const_mono_delims_exp
  | Pat => const_mono_delims_pat
  | Typ => const_mono_delims_typ
  | _ => []
  };
