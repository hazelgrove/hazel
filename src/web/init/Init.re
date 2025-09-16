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
      BasicReference.out,
      Projectors.out,
      ADTs.out,
      Tuples.out,
      Tables.out,
      Polymorphism.out,
      Cards.out,
      Probes.out,
      Livelits.out,
      // GUIDEExpressiveProgramming.out,
      // GUIDEComposingExpressions.out,
      // GUIDEComputingEquationally.out,
      // GUIDEVariables.out,
      // GUIDECompositionality.out,
      // GUIDEScope.out,
      // GUIDEShadowing.out,
      // GUIDEBoolsandTypes.out,
      // GUIDEConditionals.out,
      // GUIDEFunctions.out,
      // TESTSTypesandStaticErrors.out,
    ]
    |> List.map(((name, content)) =>
         (
           name,
           {
             editor: Editor.Model.mk_persistent(content, ~root=Exp),
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
    () => empty_cell_editor_persistent(~root=Exp),
    find_documentation_slide(name),
  );
};
