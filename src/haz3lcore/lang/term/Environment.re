[@deriving (show({with_path: false}), sexp, yojson, eq)]
type entry('a) =
  | Abstract
  | Concrete('a);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('a) = VarMap.t(entry('a));

let lookup = (x: Var.t, env: t('a)) =>
  switch (VarMap.lookup(x, env)) {
  | Some(Concrete(x)) => Some(x)
  | Some(Abstract) => None
  | None => None
  };

let mapo = (f, env) =>
  VarMap.mapo(
    ((k, y)) =>
      switch (y) {
      | Abstract => Abstract
      | Concrete(x) => Concrete(f((k, x)))
      },
    env,
  );

let empty = VarMap.empty;
let concat = VarMap.concat;
let extend = VarMap.extend;

let without_keys = (keys, env) =>
  List.fold_left((acc, k) => VarMap.extend(acc, (k, Abstract)), env, keys);
