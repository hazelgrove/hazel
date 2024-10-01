include Base.Result;

module Syntax = {
  let ( let* ) = (result, f) => bind(~f, result);
  let (let+) = (result, f) => map(~f, result);
};

module Serialization = {
  [@deriving (show, sexp, yojson)]
  type persistent('a, 'b) =
    | Ok('a)
    | Error('b);

  let to_persistent = (result: t('a, 'b)): persistent('a, 'b) =>
    switch (result) {
    | Ok(a) => Ok(a)
    | Error(b) => Error(b)
    };

  let of_persistent = (result: persistent('a, 'b)): t('a, 'b) =>
    switch (result) {
    | Ok(a) => Ok(a)
    | Error(b) => Error(b)
    };
};

let pp = (a, b, c, x) =>
  x |> Serialization.to_persistent |> Serialization.pp_persistent(a, b, c);

let t_of_yojson = (a, b, x) =>
  x |> Serialization.persistent_of_yojson(a, b) |> Serialization.of_persistent;

let yojson_of_t = (a, b, x) =>
  x |> Serialization.to_persistent |> Serialization.yojson_of_persistent(a, b);
