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
    Form.regexp("^[a-zA-Z0-9_]$", String.sub(t, 0, 1))
      ? [Mold.mk_op(Any, [])] : [Mold.mk_bin(Precedence.min, Any, [])]
  | (_lbl, None) => [Mold.mk_op(Any, [])]
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

let is_instant = kw => List.length(instant_expansion(kw) |> fst) > 1;

let append_safe = char =>
  !is_instant(char)
  && !Form.is_secondary(char)
  && !(Form.is_string_delim(char) || Form.is_comment_delim(char));

let allow_merge = (l: Token.t, r: Token.t): bool =>
  Form.is_valid_token(l ++ r)
  || !(Form.is_valid_token(l) && Form.is_valid_token(r));
// alternatively, require l++r is valid (simpler, more restrictiive)

let allow_append_right = (t: Token.t, char: string): bool =>
  Form.is_valid_token(t ++ char)
  || (
    !Form.is_valid_token(t ++ char)
    && !(Form.is_valid_token(t) && Form.is_valid_token(char))
  )
  && append_safe(char);

let allow_append_left = (char: string, t: Token.t): bool =>
  Form.is_valid_token(char ++ t)
  || (!Form.is_valid_token(t) && !Form.is_valid_token(char))
  && append_safe(char);

let allow_insertion = (char: string, t: Token.t, new_t: Token.t): bool =>
  Form.is_valid_token(new_t) || !Form.is_valid_token(t) && append_safe(char);

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
