let filled_slides = [LanguageRefSlide.lang_ref];

let empty: ScratchSlide.persistent_state = (
  1,
  {
    zipper: "((selection((focus Left)(content())))(backpack())(relatives((siblings(()((Grout((id 0)(shape Convex))))))(ancestors())))(caret Outer))",
    backup_text: "",
  },
);

let num_empty = 6;

let init_data = filled_slides @ List.init(num_empty, _ => empty);

assert(List.length(init_data) > 0);

let init = (): Editors.scratch => (
  0,
  init_data |> List.map(ScratchSlide.unpersist),
);

let init_nth = n => {
  let data = List.nth(init_data, n);
  ScratchSlide.unpersist(data);
};
