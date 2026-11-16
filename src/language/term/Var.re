open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = string;

let equal = String.equal;

/* Required by Map.Make(Var) / Set.Make(Var) */
let compare = (x: t, y: t) => compare(x, y);

/* Token.is_ctr's test of a constructor name's first letter, which may be
   any uppercase or titlecase letter (`Ä`). Token depends on Var. */
let starts_capitalized =
  StringUtil.unicode_match(StringUtil.unicode_regexp({|^[\p{Lu}\p{Lt}]|}));

/* A renamed binder must stay the same kind of name. A variable takes a
   prime (`x` to `x'`), but a capitalized name can't contain one (`A'` is
   neither a constructor nor a variable), so it counts instead: `A` to `A2`,
   `A2` to `A3`. */
let next_name = (x: t): t =>
  if (starts_capitalized(x)) {
    let len = String.length(x);
    let rec digits_start = i =>
      i > 1 && x.[i - 1] >= '0' && x.[i - 1] <= '9' ? digits_start(i - 1) : i;
    let i = digits_start(len);
    switch (int_of_string_opt(String.sub(x, i, len - i))) {
    | Some(n) => String.sub(x, 0, i) ++ string_of_int(n + 1)
    | None => x ++ "2"
    };
  } else {
    x ++ "'";
  };

let rec free_name = (x: t, bound: list(t)) =>
  if (List.mem(x, bound)) {
    free_name(next_name(x), bound);
  } else {
    x;
  };
