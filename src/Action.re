module Helper: Semantics.Core.HELPER = {
  let rec list_to_string = lst =>
    switch (lst) {
    | [] => ""
    | [a, ...rest] => string_of_int(a) ++ ", " ++ list_to_string(rest)
    };
  let path_to_string = ((lst, _)) => "[" ++ list_to_string(lst) ++ "]";
  let log_path = path => {
    JSUtil.log(path_to_string(path));
    path;
  };
  let log_nat = n => {
    JSUtil.log(n);
    n;
  };
};

include Semantics.Core.FAction(Associator, Helper);
