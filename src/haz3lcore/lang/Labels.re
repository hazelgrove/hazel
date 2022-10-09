open Util;

let paren = ["(", ")"];

let comma = [","];

let fun_ = ["fun", "->"];

let if_ = ["if", "then", "else"];
let let_ = ["let", "=", "in"];

let case = ["case", "end"];
let rule = ["|", "=>"];

let all: list(Label.t) = {
  let _ = failwith("todo complete Labels.all");
  [paren, comma, fun_, if_, let_, case, rule];
};

let by_token: StringMap.t(list(Label.t)) =
  all
  |> List.fold_left(
       (map, lbl) =>
         lbl
         |> List.fold_left(
              (map, tok) =>
                map
                |> StringMap.update(
                     tok,
                     fun
                     | None => Some([lbl])
                     | Some(lbls) => Some([lbl, ...lbls]),
                   ),
              map,
            ),
       StringMap.empty,
     );

let with_token = (tok: Token.t): list(Label.t) =>
  switch (StringMap.find_opt(tok, by_token)) {
  | None => []
  | Some(lbls) => lbls
  };
