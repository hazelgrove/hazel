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
  | (_lbl, None) =>
    //Printf.printf("MOLD NOT FOUND: %s\n", Label.show(lbl));
    [Mold.mk_op(Any, [])]
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
  !(Form.is_valid_token(l) && Form.is_valid_token(r));
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

let non_leading_delims: list(Token.t) =
  List.fold_left(
    (acc, (_, {label, _}: Form.t)) => {
      switch (label) {
      | [_, ...tl] => acc @ tl
      | _ => acc
      }
    },
    [],
    Form.forms,
  );

let leading_delims = (sort: Sort.t): list(string) => {
  // TODO(andrew): cleanup
  Form.delims
  |> List.filter_map(token => {
       let (lbl, _) = delayed_expansion(token);
       switch (get(lbl)) {
       | [a] when List.mem(token, non_leading_delims) && a.out != Any =>
         // Case for delims lile "->" which are both infix and non-leading kws
         Some(lbl)
       | _ when List.mem(token, non_leading_delims) => None
       | [a] when a.out == Any => None
       | molds =>
         Some(
           List.filter_map(
             (m: Mold.t) => m.out == sort ? Some(token) : None,
             molds,
           ),
         )
       };
     })
  |> List.flatten;
};

let lead_delims_exp = leading_delims(Exp);
let lead_delims_pat = leading_delims(Pat);
let lead_delims_typ = leading_delims(Typ);

let leading_delims = (sort: Sort.t): list(string) =>
  switch (sort) {
  | Exp => lead_delims_exp
  | Pat => lead_delims_pat
  | Typ => lead_delims_typ
  | TPat
  | Nul
  | Any
  | Rul => []
  };
