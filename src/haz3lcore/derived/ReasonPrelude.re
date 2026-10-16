/* The standard library a Reason program sees: OCaml-flavoured List,
   Option and String modules (curried, data last), written in Reason and
   wrapped around the program before statics, so Hazel's module system
   resolves `List.map` and the evaluator runs it like user code.

   There are no exceptions: `failwith` (and the partial functions built
   on it) evaluate to a hole, so a failure shows where it happened
   instead of aborting the program. Unannotated definitions get unknown
   types, which Hazel's gradual typing accepts at any type. Hazel's own
   builtins take one tuple argument, hence `string_sub((s, i, n))`. */

let source = {|let failwith = (msg: string) => ?;
let invalid_arg = (msg: string) => ?;
let ignore = (_) => ();
let min = (a, b) => if (a < b) {a} else {b};
let max = (a, b) => if (a < b) {b} else {a};
let assert = (b: bool) => if (b) {()} else {failwith("assert")};
let succ = (n) => n + 1;
let pred = (n) => n - 1;

module List = {
  let rec length = (xs) =>
    switch (xs) {
    | [] => 0
    | [_, ...rest] => 1 + length(rest)
    };
  let is_empty = (xs) =>
    switch (xs) {
    | [] => true
    | _ => false
    };
  let hd = (xs) =>
    switch (xs) {
    | [x, ..._] => x
    | [] => failwith("hd")
    };
  let tl = (xs) =>
    switch (xs) {
    | [_, ...rest] => rest
    | [] => failwith("tl")
    };
  let rec nth = (xs, n) =>
    switch (xs) {
    | [] => failwith("nth")
    | [x, ...rest] => if (n == 0) {x} else {nth(rest, n - 1)}
    };
  let rec nth_opt = (xs, n) =>
    switch (xs) {
    | [] => None
    | [x, ...rest] => if (n == 0) {Some(x)} else {nth_opt(rest, n - 1)}
    };
  let rec rev_append = (xs, acc) =>
    switch (xs) {
    | [] => acc
    | [x, ...rest] => rev_append(rest, [x, ...acc])
    };
  let rev = (xs) => rev_append(xs, []);
  let append = (xs, ys) => xs @ ys;
  let rec map = (f, xs) =>
    switch (xs) {
    | [] => []
    | [x, ...rest] => [f(x), ...map(f, rest)]
    };
  let mapi = (f, xs) => {
    let rec go = (i, xs) =>
      switch (xs) {
      | [] => []
      | [x, ...rest] => [f(i, x), ...go(i + 1, rest)]
      };
    go(0, xs);
  };
  let rev_map = (f, xs) => rev(map(f, xs));
  let rec map2 = (f, xs, ys) =>
    switch ((xs, ys)) {
    | ([x, ...xs], [y, ...ys]) => [f(x, y), ...map2(f, xs, ys)]
    | _ => []
    };
  let rev_map2 = (f, xs, ys) => rev(map2(f, xs, ys));
  let rec fold_left2 = (f, acc, xs, ys) =>
    switch ((xs, ys)) {
    | ([x, ...xs], [y, ...ys]) => fold_left2(f, f(acc, x, y), xs, ys)
    | _ => acc
    };
  let rec fold_right2 = (f, xs, ys, acc) =>
    switch ((xs, ys)) {
    | ([x, ...xs], [y, ...ys]) => f(x, y, fold_right2(f, xs, ys, acc))
    | _ => acc
    };
  let rec iter = (f, xs) =>
    switch (xs) {
    | [] => ()
    | [x, ...rest] => {
        f(x);
        iter(f, rest);
      }
    };
  let iteri = (f, xs) => {
    let rec go = (i, xs) =>
      switch (xs) {
      | [] => ()
      | [x, ...rest] => {
          f(i, x);
          go(i + 1, rest);
        }
      };
    go(0, xs);
  };
  let rec fold_left = (f, acc, xs) =>
    switch (xs) {
    | [] => acc
    | [x, ...rest] => fold_left(f, f(acc, x), rest)
    };
  let rec fold_right = (f, xs, acc) =>
    switch (xs) {
    | [] => acc
    | [x, ...rest] => f(x, fold_right(f, rest, acc))
    };
  let rec filter = (p, xs) =>
    switch (xs) {
    | [] => []
    | [x, ...rest] => if (p(x)) {[x, ...filter(p, rest)]} else {filter(p, rest)}
    };
  let rec filter_map = (f, xs) =>
    switch (xs) {
    | [] => []
    | [x, ...rest] =>
      switch (f(x)) {
      | Some(y) => [y, ...filter_map(f, rest)]
      | None => filter_map(f, rest)
      }
    };
  let rec concat = (xss) =>
    switch (xss) {
    | [] => []
    | [xs, ...rest] => xs @ concat(rest)
    };
  let flatten = concat;
  let concat_map = (f, xs) => concat(map(f, xs));
  let rec exists = (p, xs) =>
    switch (xs) {
    | [] => false
    | [x, ...rest] => p(x) || exists(p, rest)
    };
  let rec for_all = (p, xs) =>
    switch (xs) {
    | [] => true
    | [x, ...rest] => p(x) && for_all(p, rest)
    };
  let mem = (y, xs) => exists((x) => x == y, xs);
  let rec find_opt = (p, xs) =>
    switch (xs) {
    | [] => None
    | [x, ...rest] => if (p(x)) {Some(x)} else {find_opt(p, rest)}
    };
  let find = (p, xs) =>
    switch (find_opt(p, xs)) {
    | Some(x) => x
    | None => failwith("find")
    };
  let rec assoc_opt = (k, xs) =>
    switch (xs) {
    | [] => None
    | [(k2, v), ...rest] => if (k == k2) {Some(v)} else {assoc_opt(k, rest)}
    };
  let assoc = (k, xs) =>
    switch (assoc_opt(k, xs)) {
    | Some(v) => v
    | None => failwith("assoc")
    };
  let mem_assoc = (k, xs) =>
    switch (assoc_opt(k, xs)) {
    | Some(_) => true
    | None => false
    };
  let partition = (p, xs) =>
    (filter(p, xs), filter((x) => !p(x), xs));
  let rec split = (xys) =>
    switch (xys) {
    | [] => ([], [])
    | [(x, y), ...rest] =>
      let (xs, ys) = split(rest);
      ([x, ...xs], [y, ...ys]);
    };
  let rec combine = (xs, ys) =>
    switch ((xs, ys)) {
    | ([x, ...xs], [y, ...ys]) => [(x, y), ...combine(xs, ys)]
    | _ => []
    };
  let init = (n, f) => {
    let rec go = (i) => if (i >= n) {[]} else {[f(i), ...go(i + 1)]};
    go(0);
  };
  let rec insert = (cmp, x, xs) =>
    switch (xs) {
    | [] => [x]
    | [y, ...rest] => if (cmp(x, y) <= 0) {[x, y, ...rest]} else {[y, ...insert(cmp, x, rest)]}
    };
  let sort = (cmp, xs) => fold_right((x, acc) => insert(cmp, x, acc), xs, []);
};

module Option = {
  let map = (f, o) =>
    switch (o) {
    | Some(x) => Some(f(x))
    | None => None
    };
  let bind = (o, f) =>
    switch (o) {
    | Some(x) => f(x)
    | None => None
    };
  let value = (o, default) =>
    switch (o) {
    | Some(x) => x
    | None => default
    };
  let get = (o) =>
    switch (o) {
    | Some(x) => x
    | None => failwith("get")
    };
  let is_some = (o) =>
    switch (o) {
    | Some(_) => true
    | None => false
    };
  let is_none = (o) => !is_some(o);
};

module String = {
  let length = (s) => string_length(s);
  let concat = (sep, ss) => string_join((sep, ss));
  let sub = (s, start, len) => string_sub((s, start, len));
  let trim = (s) => string_trim(s);
  let uppercase_ascii = (s) => string_uppercase(s);
  let lowercase_ascii = (s) => string_lowercase(s);
  let split_on_char = (sep, s) => string_split((sep, s));
};
|};

/* The prelude parsed once, under the Reason surface: nested definitions
   whose innermost body is the closing statement's empty hole */
let term: Lazy.t(option(Language.Exp.t)) =
  lazy(
    Surface.with_(Reason, () =>
      switch (ReasonLoad.to_segment(~root=Exp, source)) {
      | seg => Some(MakeTerm.go(seg).term)
      | exception _ => None
      }
    )
  );

/* The program inside the prelude: the prelude's innermost (empty) body
   becomes the program */
let wrap = (program: Language.Exp.t): Language.Exp.t => {
  let rec go = (e: Language.Exp.t): Language.Exp.t =>
    switch (e.term) {
    | Let(p, d, body) => {
        ...e,
        term: Let(p, d, go(body)),
      }
    | TyAlias(tp, t, body) => {
        ...e,
        term: TyAlias(tp, t, go(body)),
      }
    | ModuleExp(mp, d, body) => {
        ...e,
        term: ModuleExp(mp, d, go(body)),
      }
    | _ => program
    };
  switch (Lazy.force(term)) {
  | Some(prelude) => go(prelude)
  | None => program
  };
};
