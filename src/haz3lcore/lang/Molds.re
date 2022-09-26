open Sexplib.Std;
open Util;

//  type completion =
//    | Comment((list(Token.t), Direction.t))
//    | Tile((list(Token.t), Direction.t))

[@deriving (show({with_path: false}), sexp, yojson)]
// eg [("let", (["let", "=", "in"], Left)), ("in", (["let", "=", "in"], Right))]
type completions = list((Token.t, (list(Token.t), Direction.t)));

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
  | (lbl, None) =>
    Printf.printf("MOLD NOT FOUND: %s\n", Label.show(lbl));
    [];
  };

let delayed_completes: completions =
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

// TODO append to the end of this list something like ("#", (["#", "#"], Left))
let instant_completes: completions =
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
  |> List.sort_uniq(compare)
  |> List.append([("#", (["##"], Direction.Left))]); //added

let delayed_completion:
  (Token.t, Direction.t) => (list(Token.t), Direction.t) =
  (s, direction_preference) =>
    /* Completions which must be defered as they are ambiguous prefixes */
    switch (List.assoc_opt(s, delayed_completes)) {
    | Some(completion) => completion
    | None => ([s], direction_preference)
    };

let instant_completion:
  (Token.t, Direction.t) => (list(Token.t), Direction.t) =
  (s, direction_preference) =>
    // let tile_or_comment =
    // if (s = "#") {
    //   Comment // expansion
    // };
    /* Completions which can or must be executed immediately */
    switch (List.assoc_opt(s, instant_completes)) {
    | Some(completion) => completion
    // if (s = "#") {
    //   | Comment((["#", "#"], Direction.Left))
    //   | Tile(completion)
    // };
    | None => ([s], direction_preference)
    };
