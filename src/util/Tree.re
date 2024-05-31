open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = {
  value: 'a,
  child: list(t('a)),
};

let rec fold = (f, {value, child}) =>
  f(value, child |> List.map(fold(f)));

let rec combine = ((t1, t2)) => {
  value: (t1.value, t2.value),
  child: List.combine(t1.child, t2.child) |> List.map(combine),
};

let rec map = (f, {value, child}) => {
  value: f(value),
  child: child |> List.map(map(f)),
};

let rec flaten = ({value, child}) =>
  [value] @ (child |> List.map(flaten) |> List.concat);

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Value
  | Child(int, pos);

let mapi = (f, t) => {
  let rec aux = (f, f_pos, {value, child}) => {
    value: value |> f(f_pos(Value)),
    child: child |> List.mapi(i => aux(f, pos => f_pos(Child(i, pos)))),
  };
  aux(f, Fun.id, t);
};

let rec get = ({value, child}, pos) =>
  switch (pos) {
  | Value => value
  | Child(i, pos) => pos |> get(List.nth(child, i))
  };

let set = (pos, t) => mapi((pos', t') => pos == pos' ? t : t');

let flaten_pos = t => t |> mapi((pos, _) => pos) |> flaten;

let combine_pos = t => t |> mapi((pos, t) => (pos, t));
