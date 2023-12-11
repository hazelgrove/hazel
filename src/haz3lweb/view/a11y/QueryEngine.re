open Haz3lcore;

let get_position = (id: Id.t, measured: Measured.t) =>
  switch (Measured.find_by_id(id, measured)) {
  | Some(m) => Some(m.last)
  | None => None
  };
