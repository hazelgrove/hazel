open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  current: string,
  slides: ScratchSlide.assoc,
};

let init = [
  ("Basic Reference", SerializedExamples.lang_ref),
  ("Types & errors", SerializedExamples.basic_type_egs),
  ("ADT Statics", SerializedExamples.adt_egs),
  ("ADT Dynamics", SerializedExamples.adt_dynamics_tests),
];

assert(List.length(init) > 0);

//TODO(andrew): cleanup persistents

let init_name = name => ScratchSlide.unpersist(List.assoc(name, init));

let unpersist = ((name, (id, zipper))) => {
  let (id, zipper) = PersistentZipper.unpersist(zipper, id);
  (name, (id, Editor.init(zipper, ~read_only=false)));
};
let default: t = {
  current: init |> List.hd |> fst,
  slides: List.map(unpersist, init),
};
