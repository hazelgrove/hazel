open Util_web;

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = {
  id: Haz3lcore.Id.t,
  title: string,
  module_name: string,
  prompt: string,
  max_points: int,
  prelude: Haz3lcore.Zipper.t,
  lemmas: Haz3lcore.Zipper.t,
  theorem: Haz3lcore.Zipper.t,
};

let blank_spec = (~title, ~module_name): spec => {
  id: Haz3lcore.Id.mk(),
  title,
  module_name,
  prompt: "TODO: prompt",
  max_points: 10,
  prelude: Haz3lcore.Zipper.init(),
  lemmas: Haz3lcore.Zipper.init(),
  theorem: Haz3lcore.Zipper.init(),
};
