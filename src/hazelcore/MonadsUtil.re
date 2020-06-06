let rec _bind_list = (l, binder, ret, rev_bound_vals) =>
  switch (l) {
  | [] => ret(List.rev(rev_bound_vals))
  | [x, ...xs] =>
    binder(x, bound_val =>
      _bind_list(xs, binder, ret, [bound_val, ...rev_bound_vals])
    )
  };

let bind_list = (l, binder, ret) => _bind_list(l, binder, ret, []);
let bind_count = (num, binder, ret) =>
  _bind_list(List.init(num, _ => ()), _ => binder, ret, []);
