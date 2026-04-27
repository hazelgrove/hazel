[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Type
  | Arrow(t, t);

let rec arity = (kind: t): int =>
  switch (kind) {
  | Type => 0
  | Arrow(_, result) => 1 + arity(result)
  };

let apply = (fn_kind: t, arg_kind: t): option(t) =>
  switch (fn_kind) {
  | Arrow(expected, result) when equal(expected, arg_kind) => Some(result)
  | _ => None
  };

let rec arrows = (args: list(t), result: t): t =>
  switch (args) {
  | [] => result
  | [arg, ...rest] => Arrow(arg, arrows(rest, result))
  };

let of_param_count = (count: int): t =>
  arrows(List.init(count, _ => Type), Type);

let rec to_string = (kind: t): string =>
  switch (kind) {
  | Type => "Type"
  | Arrow(Type, result) => "Type -> " ++ to_string(result)
  | Arrow(arg, result) =>
    "(" ++ to_string(arg) ++ ") -> " ++ to_string(result)
  };
