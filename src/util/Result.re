include Base.Result;

let to_option =
  fun
  | Error(_) => None
  | Ok(x) => Some(x);

module Syntax = {
  let (let+) = (result, f) => map(~f, result);
  let ( let* ) = (result, f) => bind(~f, result);
  let (let/) = (r, f) =>
    switch (r) {
    | Ok(_) => r
    | Error(e) => f(e)
    };
};
