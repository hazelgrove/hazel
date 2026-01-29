open Haz3lcore;
open Util;

let empty_cell_editor_persistent: unit => CellEditor.Model.persistent =
  () => {
    editor: Zipper.init() |> PersistentZipper.persist,
    result: EvalResult.Model.init |> EvalResult.Model.persist,
  };

let startup: PersistentData.t = {
  scratch: (0, [("Scratchpad 1", empty_cell_editor_persistent())]),
  documentation: (
    0,
    [
      BasicReference.out,
      Projectors.out,
      ADTs.out,
      Tuples.out,
      Tables.out,
      Polymorphism.out,
      Cards.out,
      Probes.out,
      Livelits.out,
      Sound.out,
    ]
    @ B2t2.Slides.all_slides
    |> List.map(((name, content: PersistentSegment.t)) =>
         (
           name,
           {
             editor: content |> PersistentSegment.unpersist,
             result: EvalResult.Model.init |> EvalResult.Model.persist,
           }: CellEditor.Model.persistent,
         )
       ),
  ),
};

let find_documentation_slide = (name: string) => {
  startup.documentation
  |> snd
  |> List.find_opt(((n, _)) => n == name)
  |> Option.map(snd);
};

let default_documentation_slide_name =
    (name: string): CellEditor.Model.persistent => {
  OptUtil.get(
    () => empty_cell_editor_persistent(),
    find_documentation_slide(name),
  );
};
