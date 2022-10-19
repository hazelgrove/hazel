let map = f =>
  fun
  | Some(x) => Some(f(x))

  | None => None;

let pure_bind = (ox, f) => map(f, ox);

let bind = (ox, f) =>
  switch (ox) {
  | Some(x) => f(x)

  | None => None
  };

let and_then = (f, ox) => bind(ox, f);

let guard = b =>
  if (b) {
    Some();
  } else {
    None;
  };

let sequence = xs => {
  let rec helper = acc =>
    fun
    | [] => Some(List.rev(acc))

    | [head, ...tail] =>
      switch (head) {
      | Some(x) => helper([x, ...acc], tail)

      | None => None
      };

  helper([], xs);
};

let with_default = (default, ox) =>
  switch (ox) {
  | Some(x) => x

  | None => default
  };

let sequence_fst = ((ox, y)) =>
  switch (ox) {
  | Some(x) => Some((x, y))

  | None => None
  };

let sequence_snd = ((x, oy)) =>
  switch (oy) {
  | Some(y) => Some((x, y))

  | None => None
  };

let filter = (pred, ox) =>
  switch (ox) {
  | Some(x) =>
    if (pred(x)) {
      Some(x);
    } else {
      None;
    }

  | None => None
  };

module Syntax = {
  let (let+) = pure_bind;
  let ( let* ) = bind;
};
