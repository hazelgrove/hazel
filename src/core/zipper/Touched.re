include Id.Map;
type t = Id.Map.t(Time.t);

module type S = {let touched: t;};

let update = (t: Time.t, es: list(Effect.t), td: t) =>
  es
  |> List.fold_left(
       (td, e: Effect.t) =>
         switch (e) {
         | Delete(id) => td |> remove(id)
         | Touch(id) =>
           td
           |> update(
                id,
                fun
                | None => Some(t)
                | Some(t') => Some(Time.max(t, t')),
              )
         },
       td,
     );

let sexp_of_t = _ => failwith("Touched.sexp_of_t");
let t_of_sexp = _ => failwith("Touched.t_of_sexp");
let yojson_of_t = _ => failwith("Touched.yojson_of_t");
let t_of_yojson = _ => failwith("Touched.t_of_yojson");
