open Haz3lcore;
open Util;

let empty_cell_editor_persistent = (~root): CellEditor.Model.persistent => {
  editor:
    Zipper.init()
    |> PersistentZipper.persist
    |> Editor.Model.mk_persistent(~root),
  result: EvalResult.Model.init |> EvalResult.Model.persist,
};

/* the world tree's slides (hazel-programs/world, generated as
   WorldSlides): a slide's path below its top folder is its name in that
   deck ("B2T2 / Errors / Malformed Tables"). Slides are module members;
   an expression program (exp) keeps an expression root. Prose is one
   TextArea projector holding the file's content */
let world_slides =
    (folder: string): list((string, Sort.t, PersistentZipper.t)) =>
  List.filter_map(
    ~f=
      (s: WorldSlides.slide) =>
        switch (s.path) {
        | [top, ...rest]
            when String.equal(top, folder) && !List.is_empty(rest) =>
          let text =
            switch (s.kind) {
            | Program => s.text
            | Prose =>
              "^^text(\"" ++ StringUtil.escape_linebreaks(s.text) ++ "\")"
            };
          Some((
            String.concat(~sep=" / ", rest),
            s.exp ? Sort.Exp : Sort.Mod,
            PersistentZipper.of_slide_text(text),
          ));
        | _ => None
        },
    WorldSlides.slides,
  );

let documentation_slides: list((string, Sort.t, PersistentZipper.t)) =
  world_slides("Documentation");

/* LAZY: the CLI links this module (--linkall) and must not pay the
   all-slides unpersist at module init; the browser forces it on first
   store access. */
let startup: Lazy.t(PersistentData.t) =
  lazy({
    scratch: (
      0,
      world_slides("Scratch")
      |> List.map(~f=((name, root, content: PersistentZipper.t)) =>
           (
             name,
             {
               editor: content |> Editor.Model.mk_persistent(~root),
               result: EvalResult.Model.init |> EvalResult.Model.persist,
             }: CellEditor.Model.persistent,
           )
         ),
    ),
    documentation: (
      0,
      documentation_slides
      |> List.map(~f=((name, root, content: PersistentZipper.t)) =>
           (
             name,
             {
               editor: content |> Editor.Model.mk_persistent(~root),
               result: EvalResult.Model.init |> EvalResult.Model.persist,
             }: CellEditor.Model.persistent,
           )
         ),
    ),
  });

let find_documentation_slide = (name: string) => {
  Lazy.force(startup).documentation
  |> snd
  |> List.find(~f=((n, _)) => String.equal(n, name))
  |> Option.map(~f=snd);
};

let default_documentation_slide_name =
    (name: string): CellEditor.Model.persistent => {
  OptUtil.get(
    () => empty_cell_editor_persistent(~root=Exp),
    find_documentation_slide(name),
  );
};

/* Derivation slides included in the Documentation mode under the
   "Derivations" section. The folder prefix makes the slide names split into
   nested dropdowns (matches how "B2T2 / ..." slides work) -- see SlidePath,
   which owns that encoding. */
let documentation_drv_slides: list((string, DerivationExercise.spec)) =
  [
    Ex_Conjunction_Commutativity.exercise,
    Ex_Curried_Function_Derivation.exercise,
    Ex_PairMap_Derivation.exercise,
    Ex_Shadowing_And_Closures.exercise,
    Ex_Type_Validation_Derivation.exercise,
  ]
  |> List.map(~f=(spec: DerivationExercise.spec) =>
       (
         SlidePath.to_string(
           SlidePath.mk(~folders=["Derivations"], spec.title),
         ),
         spec,
       )
     );

let find_documentation_drv_spec =
    (name: string): option(DerivationExercise.spec) =>
  documentation_drv_slides
  |> List.find(~f=((n, _)) => String.equal(n, name))
  |> Option.map(~f=snd);

let documentation_drv_slide_names = (): list(string) =>
  List.map(~f=fst, documentation_drv_slides);
