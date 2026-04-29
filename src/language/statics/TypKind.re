open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Type
  | Arrow(list(t), t);

/* Total number of arguments accepted across the (possibly tuple-typed)
   argument list and any residual arrow in the result. For
   `Arrow([Type, Type], Type)` this is 2; for `Arrow([Type], Arrow([Type], Type))`
   it is also 2 (currying is allowed at the kind level). */
let rec arity = (kind: t): int =>
  switch (kind) {
  | Type => 0
  | Arrow(args, result) => List.length(args) + arity(result)
  };

/* Apply a kind to a *single* argument's kind. Tuple-arrow kinds are
   atomic: a kind `Arrow([k1, k2], r)` requires *both* arguments at once
   and cannot be partially applied to one of them. So this only
   succeeds for single-argument arrows `Arrow([k], r)`. Multi-argument
   applications go through `apply_all` instead. */
let apply = (fn_kind: t, arg_kind: t): option(t) =>
  switch (fn_kind) {
  | Arrow([expected], result) when equal(expected, arg_kind) => Some(result)
  | _ => None
  };

/* Apply a kind to a tuple of argument kinds at once, consuming the
   entire argument list of a tuple-arrow kind. Fails on arity mismatch
   or kind mismatch. */
let apply_all = (fn_kind: t, arg_kinds: list(t)): option(t) =>
  switch (fn_kind) {
  | Arrow(expected, result)
      when
        List.length(expected) == List.length(arg_kinds)
        && List.for_all2(equal, expected, arg_kinds) =>
    Some(result)
  | _ => None
  };

let arrows = (args: list(t), result: t): t =>
  switch (args) {
  | [] => result
  | _ => Arrow(args, result)
  };

let of_param_count = (count: int): t =>
  arrows(List.init(count, _ => Type), Type);

let rec to_string = (kind: t): string =>
  switch (kind) {
  | Type => "Type"
  | Arrow([], result) => to_string(result)
  | Arrow([arg], result) =>
    let arg_str = to_string(arg);
    let arg_str =
      switch (arg) {
      | Arrow(_, _) => "(" ++ arg_str ++ ")"
      | _ => arg_str
      };
    arg_str ++ " -> " ++ to_string(result);
  | Arrow(args, result) =>
    "("
    ++ String.concat(", ", List.map(to_string, args))
    ++ ") -> "
    ++ to_string(result)
  };
