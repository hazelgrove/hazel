include Base.Result;

let ok_or_fail = s =>
  fun
  | Error(_) => failwith(s)
  | Ok(ok) => ok;

let to_option =
  fun
  | Error(_) => None
  | Ok(x) => Some(x);

let unwrap =
  fun
  | Error(a)
  | Ok(a) => a;

let compare = (_, _, _) => failwith("todo result compare");

[@warning "-27"]
let pick = (~compare: ('ok, 'ok) => int, rs: list(t('ok, _))): t('ok, _) =>
  failwith("todo Molder.Result.pick");

module Syntax = {
  let (let+) = (result, f) => map(~f, result);
  let ( let* ) = (result, f) => bind(~f, result);
  let (let/) = (r, f) =>
    switch (r) {
    | Ok(ok) => Ok(ok)
    | Error(err) => f(err)
    };
};
