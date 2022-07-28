open Grain;

let here = "hazel";
let amend = (prepend, path) =>
  path |> Path.strip_root' |> Path.append(prepend |> Path.v);

module Stub = (M: Make.M) =>
  Make.Stub({
    let path = amend(here, M.path);
  });

module Gen = (M: Make.M) =>
  Make.Gen({
    let path = amend(here, M.path);
  });
