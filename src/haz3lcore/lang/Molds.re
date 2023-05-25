open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type expansions = list((Token.t, (list(Token.t), Direction.t)));

let forms_assoc: list((string, Form.t)) => list((Label.t, list(Mold.t))) =
  forms =>
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
      forms,
    );

let get = (label: Label.t): list(Mold.t) => {
  let t = List.hd(label);
  let forms =
    if (Form.is_op(t)) {
      [
        (
          "user_op",
          (Form.is_op, [Mold.mk_bin(Form.prec_of_op(t), Exp, [])]),
        ),
      ]
      @ List.tl(Form.atomic_forms);
    } else {
      Form.atomic_forms;
    };
  switch (label, List.assoc_opt(label, forms_assoc(Form.forms))) {
  | ([t], Some(molds)) when Form.atomic_molds(t, forms) != [] =>
    // TODO(andrew): does this make sense?
    Form.atomic_molds(t, forms) @ molds
  | ([t], None) when Form.atomic_molds(t, forms) != [] =>
    Form.atomic_molds(t, forms)
  | (_, Some(molds)) => molds
  | (lbl, None) =>
    Printf.printf("MOLD NOT FOUND: %s\n", Label.show(lbl));
    [];
  };
};

let delayed_expansions: expansions = {
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
};

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
