open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  idx: int,
  slides: ScratchSlide.s,
};

let filled_slides = [SerializedExamples.intro];

let empty: ScratchSlide.persistent_state = (
  1,
  {
    zipper: "((selection((focus Left)(content())))(backpack())(relatives((siblings(()((Grout((id 0)(shape Convex))))))(ancestors())))(caret Outer))",
    backup_text: "",
  },
);

let num_empty = 8;

let init_slides = filled_slides @ List.init(num_empty, _ => empty);
assert(List.length(init_slides) > 0);

let default: t = {
  idx: 0,
  slides: List.map(ScratchSlide.unpersist, init_slides),
};

let init_nth = n => {
  let data = List.nth(init_slides, n);
  ScratchSlide.unpersist(data);
};
