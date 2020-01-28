exception No_value;

let get = o =>
  switch (o) {
  | None => raise(No_value)
  | Some(v) => v
  };
