let uncons =
  fun
  | "" => None
  | s => Some(String.(sub(s, 0, 1), sub(s, 1, length(s) - 1)));
let unsnoc =
  fun
  | "" => None
  | s => {
      let n = String.length(s);
      Some(String.(sub(s, 0, n - 1), sub(s, n - 1, 1)));
    };

let cat = String.concat("");

let remove_nth = (n, t) => {
  assert(n < String.length(t));
  String.sub(t, 0, n) ++ String.sub(t, n + 1, String.length(t) - n - 1);
};

let remove_first = remove_nth(0);
let remove_last = t => remove_nth(String.length(t) - 1, t);

let insert_nth = (n, s, t) => {
  assert(n < String.length(t));
  String.sub(t, 0, n) ++ s ++ String.sub(t, n, String.length(t) - n);
};

let unzip = (n, t) => String.(sub(t, 0, n), sub(t, n, length(t) - n));
let unzip_opt = (n, t) =>
  switch (unzip(n, t)) {
  | (l, r) => Some((l, r))
  | exception (Invalid_argument(_)) => None
  };

let to_list = s => List.init(String.length(s), i => String.make(1, s.[i]));

let repeat = (n, s) => String.concat("", List.init(n, _ => s));
