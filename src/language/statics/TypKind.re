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
  | (Unknown, _)
  | (_, Unknown) => true
  | (Type, Type) => true
  | (Type, _)
  | (_, Type) => false
  | (Arrow(args1, r1), Arrow(args2, r2)) =>
    List.length(args1) == List.length(args2)
    && List.for_all2(consistent, args1, args2)
    && consistent(r1, r2)
  };

/* Total number of arguments accepted across the (possibly tuple-typed)
   argument list and any residual arrow in the result. For
   `Arrow([Type, Type], Type)` this is 2; for `Arrow([Type], Arrow([Type], Type))`
   it is also 2 (currying is allowed at the kind level). */
let rec arity = (kind: t): int =>
  switch (kind) {
  | Unknown
  | Type => 0
  | Arrow(args, result) => List.length(args) + arity(result)
  };

/* Apply a kind to a *single* argument's kind. Tuple-arrow kinds are
   atomic: a kind `Arrow([k1, k2], r)` requires *both* arguments at once
   and cannot be partially applied to one of them. So this only
   succeeds for single-argument arrows `Arrow([k], r)`. Multi-argument
   applications go through `apply_all` instead. Applying through an
   `Unknown` callee yields `Unknown` (no constraint). */
let apply = (fn_kind: t, arg_kind: t): option(t) =>
  switch (fn_kind) {
  | Unknown => Some(Unknown)
  | Arrow([expected], result) when consistent(expected, arg_kind) =>
    Some(result)
  | _ => None
  };

/* Apply a kind to a tuple of argument kinds at once, consuming the
   entire argument list of a tuple-arrow kind. Fails on arity mismatch
   or kind mismatch. `Unknown` callee absorbs any number of arguments. */
let apply_all = (fn_kind: t, arg_kinds: list(t)): option(t) =>
  switch (fn_kind) {
  | Unknown => Some(Unknown)
  | Arrow(expected, result)
      when
        List.length(expected) == List.length(arg_kinds)
        && List.for_all2(consistent, expected, arg_kinds) =>
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
