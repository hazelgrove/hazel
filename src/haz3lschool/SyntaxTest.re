open Haz3lcore;

type params = {
  var_mention: list(string),
  recursive: list(string),
};

let rec find_var = (uexp: Term.UExp.t, name: string): bool => {
  switch (uexp.term) {
  | Var(x) => x == name
  | Fun(_, body) => find_var(body, name)
  | Tuple(l) =>
    List.fold_left((acc, es) => {acc || find_var(es, name)}, false, l)
  | Let(_, def, body) => find_var(def, name) || find_var(body, name)
  | Ap(u1, u2) => find_var(u1, name) || find_var(u2, name)
  | If(u1, u2, u3) =>
    find_var(u1, name) || find_var(u2, name) || find_var(u3, name)
  | Seq(u1, u2) => find_var(u1, name) || find_var(u2, name)
  | Test(u) => find_var(u, name)
  | Parens(u) => find_var(u, name)
  | Cons(u1, u2) => find_var(u1, name) || find_var(u2, name)
  | UnOp(_, u) => find_var(u, name)
  | BinOp(_, u1, u2) => find_var(u1, name) || find_var(u2, name)
  | Match(g, l) =>
    find_var(g, name)
    || List.fold_left(
         (acc, pe) => {
           let (_, u) = pe;
           acc || find_var(u, name);
         },
         false,
         l,
       )
  | _ => false
  };
};

let check = (uexp: Term.UExp.t, p: params): bool => {
  List.fold_left(
    (acc, name) => {acc || find_var(uexp, name)},
    false,
    p.var_mention,
  );
};
