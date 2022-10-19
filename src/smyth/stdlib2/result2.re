let map = f =>
  fun
  | Ok(x) => Ok(f(x))

  | Error(e) => Error(e);

let pure_bind = (x, f) => map(f, x);

let bind = (rx, f) =>
  switch (rx) {
  | Ok(x) => f(x)

  | Error(e) => Error(e)
  };

let and_then = (f, rx) => bind(rx, f);

let guard = (e, b) =>
  if (b) {
    Ok();
  } else {
    Error(e);
  };

let sequence = xs => {
  let rec helper = acc =>
    fun
    | [] => Ok(List.rev(acc))

    | [head, ...tail] =>
      switch (head) {
      | Ok(x) => helper([x, ...acc], tail)

      | Error(e) => Error(e)
      };

  helper([], xs);
};

let to_option = r =>
  switch (r) {
  | Ok(x) => Some(x)

  | Error(_) => None
  };

let with_default = (default, r) =>
  switch (r) {
  | Ok(x) => x

  | Error(_) => default
  };

let unwrap = (f, g, r) =>
  switch (r) {
  | Ok(x) => f(x)

  | Error(y) => g(y)
  };

module Syntax = {
  let (let+) = pure_bind;
  let ( let* ) = bind;
};
