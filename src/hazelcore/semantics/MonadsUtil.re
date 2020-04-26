let rec _bind_many = (num, binder, ret, rev_bound_vals) =>
  if (num < 0) {
    failwith("invalid negative num = " ++ string_of_int(num));
  } else if (num == 0) {
    ret(List.rev(rev_bound_vals));
  } else {
    binder(bound_val =>
      _bind_many(num - 1, binder, ret, [bound_val, ...rev_bound_vals])
    );
  };

let bind_many = (num, binder, ret) => _bind_many(num, binder, ret, []);
