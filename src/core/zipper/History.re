module Time = {
  type t = int;
  let t = ref(0);

  let tick = (): t => {
    let time = t^;
    t := time + 1;
    time;
  };

  let last = () => {
    t^ - 1;
  };

  let lt = (<);

  let min = min;
  let max_time = Int.max_int;
};

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

let just_touched = (id: Id.t): bool =>
  switch (Id.Map.find_opt(id, t^)) {
  | None => false
  | Some(r) => r.modified >= Time.last()
  };
