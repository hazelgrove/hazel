open Lang;

let rec syntactically_equal: (pat, pat) => bool = (
  (p1, p2) =>
    switch (p1, p2) {
    | (PVar(x1), PVar(x2)) => String.equal(x1, x2)

    | (PTuple(ps1), PTuple(ps2)) =>
      Int.equal(List.length(ps1), List.length(ps2))
      && List.for_all2(syntactically_equal, ps1, ps2)

    | (PWildcard, PWildcard) => true

    | _ => false
    }:
    (pat, pat) => bool
);

let rec bind_res: (pat, res) => option(env) = (
  (p, r) =>
    switch (p) {
    | PVar(x) => Some(Env.add_res((x, r), Env.empty))

    | PTuple(ps) =>
      switch (r) {
      | RTuple(rs) =>
        if (Int.equal(List.length(ps), List.length(rs))) {
          List.map2(bind_res, ps, rs)
          |> Option2.sequence
          |> Option2.map(Env.concat);
        } else {
          None;
        }

      | _ =>
        let len = List.length(ps);

        ps
        |> List.mapi((i_, p) => bind_res(p, RProj(len, i_ + 1, r)))
        |> Option2.sequence
        |> Option2.map(Env.concat);
      }

    | PWildcard => Some(Env.empty)
    }:
    (pat, res) => option(env)
);

let bind_rec_name_res: (option(string), res) => env = (
  (rec_name_opt, r) =>
    switch (rec_name_opt) {
    | Some(rec_name) => Env.add_res((rec_name, r), Env.empty)

    | None => Env.empty
    }:
    (option(string), res) => env
);

let rec bind_typ: (bind_spec, pat, typ) => option(type_ctx) = (
  (bind_spec, p, tau) =>
    switch (p) {
    | PVar(x) =>
      Some(Type_ctx.add_type((x, (tau, bind_spec)), Type_ctx.empty))

    | PTuple(ps) =>
      switch (tau) {
      | TTuple(taus) =>
        if (Int.equal(List.length(ps), List.length(taus))) {
          List.map2(bind_typ(bind_spec), ps, taus)
          |> Option2.sequence
          |> Option2.map(Type_ctx.concat);
        } else {
          None;
        }

      | _ => None
      }

    | PWildcard => Some(Type_ctx.empty)
    }:
    (bind_spec, pat, typ) => option(type_ctx)
);

let bind_rec_name_typ: (option(string), typ) => type_ctx = (
  (rec_name_opt, tau) =>
    switch (rec_name_opt) {
    | Some(rec_name) =>
      Type_ctx.add_type((rec_name, (tau, Rec(rec_name))), Type_ctx.empty)

    | None => Type_ctx.empty
    }:
    (option(string), typ) => type_ctx
);
