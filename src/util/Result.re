include Base.Result;

module Syntax = {
  let ( let* ) = (result, f) => bind(~f, result);
  let (let+) = (result, f) => map(~f, result);
};
