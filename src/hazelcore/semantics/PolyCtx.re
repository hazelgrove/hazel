type elem =
  | VarBind(Var.t, HTyp.t)
  | VarName(Var.t)
  | EVar(MetaVar.t)
  | EVarSolved(MetaVar.t, HTyp.t)
  | Marker(MetaVar.t);

type t = list(elem);

let empty = [];

let is_empty =
  fun
  | [] => true
  | [_, ..._] => false;

let drop_after = (ctx: t, x: MetaVar.t): t => {
  let rec loop = (ctx: t, x: MetaVar.t, acc: t): t =>
    switch (ctx) {
    | [] => List.rev(acc)
    | [Marker(_x), ..._] => List.rev(acc)
    | [hd, ...rest] => loop(rest, x, [hd, ...acc])
    };
  loop(ctx, x, []);
};

let e_eq = (e, e') =>
  switch (e, e') {
  | (VarBind(x, t), VarBind(x', t')) => Var.eq(x, x') && HTyp.equiv(t, t')
  | (VarName(x), VarName(y)) => Var.eq(x, y)
  | (EVar(u), EVar(v)) => MetaVar.eq(u, v)
  | (EVarSolved(u, t), EVarSolved(u', t')) =>
    MetaVar.eq(u, u') && HTyp.equiv(t, t')
  | (Marker(u), Marker(v)) => MetaVar.eq(u, v)
  | (_, _) => false
  };

let is_solution_of = (u, e) =>
  switch (e) {
  | EVarSolved(u', _) => MetaVar.eq(u, u')
  | _ => false
  };
