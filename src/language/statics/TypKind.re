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

/* Apply a single argument to a kind. Curried application against a tuple
   arrow consumes one argument from the head of the list:
     `Arrow([k0, k1, ...], r) ⊳ k0  ↦  Arrow([k1, ...], r)`
   When the argument list becomes empty the residual is just `r`. */
let apply = (fn_kind: t, arg_kind: t): option(t) =>
  switch (fn_kind) {
  | Arrow([expected, ...rest], result) when equal(expected, arg_kind) =>
    switch (rest) {
    | [] => Some(result)
    | _ => Some(Arrow(rest, result))
    }
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
