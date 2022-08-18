module Record = {
  type t = {
    created: Time.t,
    modified: Time.t,
  };
};
type t = Id.Map.t(Record.t);

let t = ref(Id.Map.empty);

let get = (id: Id.t): Record.t => Id.Map.find(id, t^);

let touch = (id: Id.t) => {
  let time = Time.tick();
  t :=
    t^
    |> Id.Map.update(
         id,
         fun
         | None => Some(Record.{created: time, modified: time})
         | Some(r) => Some({...r, modified: time}),
       );
};

let touch_s = (ids: list(Id.t)) => {
  let time = Time.tick();
  t :=
    ids
    |> List.fold_left(
         (t, id) =>
           t
           |> Id.Map.update(
                id,
                fun
                | None => Some(Record.{created: time, modified: time})
                | Some(r) => Some({...r, modified: time}),
              ),
         t^,
       );
};
