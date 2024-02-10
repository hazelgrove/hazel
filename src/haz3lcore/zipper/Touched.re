include Id.Map;
type t = Id.Map.t(Time.t);

module type S = {
  let touched: t;
};

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
