open Util;

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

/* String-based counterpart of [spec], for "transitionary" exercise modules
 * that survive Zipper.t datatype changes (each editor is program text,
 * re-parsed at load time). Mirrors CodeExercise.transitionary_spec. */
[@deriving (show({with_path: false}), sexp, yojson)]
type transitionary_spec = {
  id: Haz3lcore.Id.t,
  title: string,
  module_name: string,
  prompt: string,
  max_points: int,
  prelude: string,
  lemmas: string,
  theorem: string,
};

let zipper_of_code = code =>
  switch (Haz3lcore.Parser.to_zipper(code, ~root=Haz3lcore.Sort.Exp)) {
  | None => failwith("Transition failed.")
  | Some(zipper) => zipper
  };

let transition = (t: transitionary_spec): spec => {
  id: t.id,
  title: t.title,
  module_name: t.module_name,
  prompt: t.prompt,
  max_points: t.max_points,
  prelude: zipper_of_code(t.prelude),
  lemmas: zipper_of_code(t.lemmas),
  theorem: zipper_of_code(t.theorem),
};

/* Persistent counterpart of [spec] (see CodeExercise.persistent_spec):
 * serialized zippers with plaintext fallback; the shipped format for
 * example modules. */
[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_spec = {
  id: Haz3lcore.Id.t,
  title: string,
  module_name: string,
  prompt: string,
  max_points: int,
  prelude: Haz3lcore.PersistentZipper.t,
  lemmas: Haz3lcore.PersistentZipper.t,
  theorem: Haz3lcore.PersistentZipper.t,
};

let of_persistent = (t: persistent_spec): spec => {
  let unpersist =
    Haz3lcore.PersistentZipper.unpersist(~root=Haz3lcore.Sort.Exp);
  {
    id: t.id,
    title: t.title,
    module_name: t.module_name,
    prompt: t.prompt,
    max_points: t.max_points,
    prelude: unpersist(t.prelude),
    lemmas: unpersist(t.lemmas),
    theorem: unpersist(t.theorem),
  };
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
