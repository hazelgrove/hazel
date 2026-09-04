open Haz3lcore;
open Util_web;

let empty_cell_editor_persistent = (~root): CellEditor.Model.persistent => {
  editor:
    Zipper.init()
    |> PersistentZipper.persist
    |> Editor.Model.mk_persistent(~root),
  result: EvalResult.Model.init |> EvalResult.Model.persist,
};

let documentation_slides: list((string, PersistentZipper.t)) =
  Docslides.Slides.all_slides @ B2t2.Slides.all_slides;

/* LAZY: the CLI links this module (--linkall) and must not pay the
   all-slides unpersist at module init; the browser forces it on first
   store access. */
let startup: Lazy.t(PersistentData.t) =
  lazy({
    scratch: (
      0,
      [("Scratchpad 1", empty_cell_editor_persistent(~root=Exp))],
    ),
    documentation: (
      0,
      documentation_slides
      |> List.map(((name, content: PersistentZipper.t)) =>
           (
             name,
             {
               editor: content |> Editor.Model.mk_persistent(~root=Exp),
               result: EvalResult.Model.init |> EvalResult.Model.persist,
             }: CellEditor.Model.persistent,
           )
         ),
    ),
  });

let find_documentation_slide = (name: string) => {
  Lazy.force(startup).documentation
  |> snd
  |> List.find_opt(((n, _)) => n == name)
  |> Option.map(snd);
};

let default_documentation_slide_name =
    (name: string): CellEditor.Model.persistent => {
  OptUtil.get(
    () => empty_cell_editor_persistent(~root=Exp),
    find_documentation_slide(name),
  );
};

/* Derivation slides included in the Documentation mode under the
   "Derivations" section. The section prefix makes the slide names split
   into nested dropdowns (matches how "B2T2 / ..." slides work). */
let documentation_drv_slides: list((string, DerivationExercise.spec)) =
  [
    Ex_Conjunction_Commutativity.exercise,
    Ex_Curried_Function_Derivation.exercise,
    Ex_PairMap_Derivation.exercise,
    Ex_Shadowing_And_Closures.exercise,
    Ex_Type_Validation_Derivation.exercise,
  ]
  |> List.map((spec: DerivationExercise.spec) =>
       ("Derivations / " ++ spec.title, spec)
     );

let find_documentation_drv_spec =
    (name: string): option(DerivationExercise.spec) =>
  documentation_drv_slides
  |> List.find_opt(((n, _)) => n == name)
  |> Option.map(snd);

let documentation_drv_slide_names = (): list(string) =>
  List.map(fst, documentation_drv_slides);
