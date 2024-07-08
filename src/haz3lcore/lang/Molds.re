open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;
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

let allow_merge = (l: Token.t, r: Token.t): bool =>
  Form.is_potential_token(l ++ r);

let allow_append_right = (t: Token.t, char: string): bool =>
  Form.is_potential_token(t ++ char);

let allow_append_left = (char: string, t: Token.t): bool =>
  Form.is_potential_token(t ++ char);

let allow_insertion = (_char: string, _t: Token.t, new_t: Token.t): bool =>
  Form.is_potential_token(new_t);
