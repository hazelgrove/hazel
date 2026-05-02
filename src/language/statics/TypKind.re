open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Unknown /* unconstrained — used for unbound type variables and
              wherever we'd otherwise have to assume a kind without
              evidence; consistent with every other kind. */
  | Type
  | Arrow(list(t), t);

/* Two kinds are *consistent* if they could have the same shape under
   some refinement of `Unknown`s. `Unknown` is consistent with anything;
   everything else is consistent only with itself (modulo recursing
   into `Arrow`'s slots). */
let rec consistent = (k1: t, k2: t): bool =>
  switch (k1, k2) {
  | (Unknown, Unknown | Type | Arrow(_, _))
  | (Type | Arrow(_, _), Unknown) => true
  | (Type, Type) => true
  | (Type, Arrow(_, _))
  | (Arrow(_, _), Type) => false
  | (Arrow(args1, r1), Arrow(args2, r2)) =>
    List.length(args1) == List.length(args2)
    && List.for_all2(consistent, args1, args2)
    && consistent(r1, r2)
  };

/* Apply a kind to a tuple of argument kinds, consuming the entire
   argument list of a tuple-arrow kind in one step. Fails on arity
   mismatch or kind mismatch. `Unknown` callee absorbs any number of
   arguments. `Type` is not callable. */
let apply_all = (fn_kind: t, arg_kinds: list(t)): option(t) =>
  switch (fn_kind) {
  | Unknown => Some(Unknown)
  | Type => None
  | Arrow(expected, result) =>
    List.length(expected) == List.length(arg_kinds)
    && List.for_all2(consistent, expected, arg_kinds)
      ? Some(result) : None
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
  | Unknown => "?"
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
