open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Haz3lcore.Id.t,
  title: string,
  prompt: string,
  prelude: Haz3lcore.Zipper.t,
  lemmas: Haz3lcore.Zipper.t,
  theorem: Haz3lcore.Zipper.t,
};
