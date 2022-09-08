include Id.Map;
type t = Id.Map.t(Time.t);

module type S = {let touched: t;};

let update = (t: Time.t, es: Id.Map.t(list(Effect.t)), td: t) =>
  Id.Map.bindings(es)
  |> List.fold_left(
       (td, (_, effs)) =>
         effs
         |> List.fold_left(
              (td, e: Effect.t) =>
                switch (e) {
                | Remold(_) => td
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
            ),
       td,
     );
