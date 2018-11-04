module Helper: Semantics.HELPER = {
  let rec list_to_string = lst =>
    switch (lst) {
    | [] => ""
    | [a, ...rest] => string_of_int(a) ++ ", " ++ list_to_string(rest)
    };
  let path_to_string = ((lst, _)) => "[" ++ list_to_string(lst) ++ "]";
  let string_of_nat = n => string_of_int(n);
  let log_nat = n => {
    JSUtil.log(n);
    n;
  };
  let log_string = s => {
    JSUtil.log(s);
    s;
  };
};
