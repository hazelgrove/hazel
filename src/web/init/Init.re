open Haz3lcore;

let startup: PersistentData.t = {
  scratch: (
    0,
    [
      (
        "Scratchpad 1",
        {
          editor: Zipper.init() |> PersistentZipper.persist,
          result: EvalResult.Model.init |> EvalResult.Model.persist,
        },
      ),
    ],
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
      GUIDEExpressiveProgramming.out,
      GUIDEComposingExpressions.out,
      GUIDEComputingEquationally.out,
      GUIDEVariables.out,
      GUIDECompositionality.out,
      GUIDEScope.out,
      GUIDEShadowing.out,
      GUIDEBoolsandTypes.out,
      GUIDEConditionals.out,
      GUIDEFunctions.out,
      TESTSTypesandStaticErrors.out,
    ]
    |> List.map(((name, content)) =>
         (
           name,
           {
             editor: content,
             result: EvalResult.Model.init |> EvalResult.Model.persist,
           }: CellEditor.Model.persistent,
         )
       ),
  ),
  configuration: (0, [Colors.out]),
};
