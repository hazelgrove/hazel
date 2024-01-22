let filled_slides = [];

let empty: ScratchSlide.persistent_state = {
  zipper: "((selection((focus Left)(content())))(backpack())(relatives((siblings(()((Grout((id 0)(shape Convex))))))(ancestors())))(caret Outer))",
  backup_text: "",
};

let num_empty = 8;

let init_data = filled_slides @ List.init(num_empty, _ => empty);

assert(List.length(init_data) > 0);

let init = (~inference_enabled) => (
  0,
  init_data |> List.map(ScratchSlide.unpersist(~inference_enabled)),
);

let init_nth = n => {
  let data = List.nth(init_data, n);
  ScratchSlide.unpersist(data);
};
