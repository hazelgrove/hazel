open Haz3lcore;
open Util;

let empty_cell_editor_persistent = (~root): CellEditor.Model.persistent => {
  editor:
    Zipper.init()
    |> PersistentZipper.persist
    |> Editor.Model.mk_persistent(~root),
  result: EvalResult.Model.init |> EvalResult.Model.persist,
};

let startup: PersistentData.t = {
  scratch: (
    0,
    [("Scratchpad 1", empty_cell_editor_persistent(~root=Exp))],
  ),
  documentation: (
    0,
    [
      // BasicReference.out,
      // Projectors.out,
      ADTs.out,
      // Tuples.out,
      // Modules.out,
      // Tables.out,
      // Polymorphism.out,
      // Cards.out,
      Probes.out,
      // Livelits.out,
    ]
    //@ B2t2.Slides.all_slides
    @ Study.AllStudy.all
    |> List.map(((name, content: PersistentSegment.t)) =>
         (
           name,
           {
             editor:
               content
               |> PersistentSegment.unpersist(~root=Exp)
               |> Editor.Model.mk_persistent(~root=Exp),
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

/* Cache of original documentation slide segments for fast comparison.
   Computed lazily on first access to avoid startup cost.

   This cache exists to optimize the "don't save unchanged slides" check
   in ScratchMode.persist. That check compares current segments to originals,
   and without caching, it was re-parsing (unpersisting) every original slide
   on every autosave.

   This whole mechanism (comparing to originals to avoid saving) might be
   unnecessary if we instead tracked dirty state per-slide, or if we moved
   to per-slide localStorage keys instead of one big blob. See the save
   system discussion in the codebase for future cleanup opportunities. */
let original_doc_segments: ref(option(Maps.StringMap.t(Segment.t))) =
  ref(None);

let get_original_doc_segment = (name: string): option(Segment.t) => {
  let cache =
    switch (original_doc_segments^) {
    | Some(c) => c
    | None =>
      let c =
        startup.documentation
        |> snd
        |> List.map(((n, pce: CellEditor.Model.persistent)) =>
             (
               n,
               pce.editor.zipper
               |> PersistentZipper.unpersist(~root=pce.editor.root)
               |> Zipper.zip,
             )
           )
        |> List.to_seq
        |> Maps.StringMap.of_seq;
      original_doc_segments := Some(c);
      c;
    };
  Maps.StringMap.find_opt(name, cache);
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
