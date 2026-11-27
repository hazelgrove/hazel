open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = string;

let equal = String.equal;

/* Required by Map.Make(Var) / Set.Make(Var) */
let compare = (x: t, y: t) => String.compare(x, y);

/* Token.is_ctr's test of a constructor name's first letter, which may be
   any uppercase or titlecase letter (`Ä`). Token depends on Var. */
let starts_capitalized =
  StringUtil.unicode_match(StringUtil.unicode_regexp({|^[\p{Lu}\p{Lt}]|}));

/* A renamed binder must stay the same kind of name. A variable takes a
   prime (`x` to `x'`), but a capitalized name can't contain one (`A'` is
   neither a constructor nor a variable), so it counts instead: `A` to `A2`,
   `A2` to `A3`. A count that can't go up appends a `2`. */
let next_name = (x: t): t =>
  if (starts_capitalized(x)) {
    let len = String.length(x);
    let rec digits_start = i =>
      i > 1 && Char.is_digit(x.[i - 1]) ? digits_start(i - 1) : i;
    let i = digits_start(len);
    switch (Int.of_string_opt(String.sub(x, ~pos=i, ~len=len - i))) {
    | Some(n) when n < Int.max_value =>
      String.sub(x, ~pos=0, ~len=i) ++ Int.to_string(n + 1)
    | Some(_)
    | None => x ++ "2"
    };
  } else {
    x ++ "'";
  };

let rec free_name = (x: t, bound: list(t)) =>
  if (List.mem(bound, x, ~equal)) {
    free_name(next_name(x), bound);
  } else {
    x;
  };
