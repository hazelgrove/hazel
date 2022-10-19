open Lang;

let empty: type_ctx = (([], []): type_ctx);

let all_type: type_ctx => list(type_binding) = (
  ((ts, _)) => ts: type_ctx => list(type_binding)
);

let all_poly: type_ctx => list(string) = (
  ((_, ps)) => ps: type_ctx => list(string)
);

let concat: list(type_ctx) => type_ctx = (
  gammas => {
    let (tss, pss) = List.split(gammas);

    (List.concat(tss), List.concat(pss));
  }:
    list(type_ctx) => type_ctx
);

let add_type: (type_binding, type_ctx) => type_ctx = (
  (b, (ts, ps)) => ([b, ...ts], ps): (type_binding, type_ctx) => type_ctx
);

let concat_type: (list(type_binding), type_ctx) => type_ctx = (
  (bs, (ts, ps)) => (bs @ ts, ps):
    (list(type_binding), type_ctx) => type_ctx
);

let add_poly: (poly_binding, type_ctx) => type_ctx = (
  (b, (ts, ps)) => (ts, [b, ...ps]): (poly_binding, type_ctx) => type_ctx
);

let concat_poly: (list(poly_binding), type_ctx) => type_ctx = (
  (bs, (ts, ps)) => (ts, bs @ ps):
    (list(poly_binding), type_ctx) => type_ctx
);

let peel_type: type_ctx => option((type_binding, type_ctx)) = (
  ((ts, ps)) =>
    switch (ts) {
    | [head, ...tail] => Some((head, (tail, ps)))

    | [] => None
    }:
    type_ctx => option((type_binding, type_ctx))
);

let names: type_ctx => list(string) = (
  ((ts, ps)) => List.map(fst, ts) @ ps: type_ctx => list(string)
);
