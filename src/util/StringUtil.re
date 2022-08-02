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

let split_nth = (n, t) => {
  assert(n < String.length(t));
  (String.sub(t, 0, n), String.sub(t, n, String.length(t) - n));
};

let to_list = s => List.init(String.length(s), i => String.make(1, s.[i]));

let repeat = (n, s) => String.concat("", List.init(n, _ => s));
