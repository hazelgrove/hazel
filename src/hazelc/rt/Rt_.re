let here = "rt";

module Stub = (M: Make.M) =>
  Hazel_.Stub({
    let path = Hazel_.amend(here, M.path);
  });

module Gen = (M: Make.M) =>
  Hazel_.Gen({
    let path = Hazel_.amend(here, M.path);
  });
