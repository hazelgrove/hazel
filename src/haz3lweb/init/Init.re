open Haz3lcore;

let startup: PersistentData.t = {
  scratch: (0, List.init(8, _ => Zipper.init() |> PersistentZipper.persist)),
  documentation: (
    0,
    [
      ("Basic Reference", BasicReference.out),
      ("Projectors", Projectors.out),
      ("ADTs", ADTs.out),
      ("Polymorphism", Polymorphism.out),
      ("Cards", Cards.out),
      ("Probes", Probes.out),
      ("[GUIDE] Expressive Programming", ExpressiveProgramming.out),
      ("[GUIDE] Composing Expressions", ComposingExpressions.out),
      ("[GUIDE] Computing Equationally", ComputingEquationally.out),
      ("[GUIDE] Variables", Variables.out),
      ("[GUIDE] Compositionality", Compositionality.out),
      ("[GUIDE] Scope", Scope.out),
      ("[GUIDE] Shadowing", Shadowing.out),
      ("[GUIDE] Bools and Types", BoolsAndTypes.out),
      ("[GUIDE] Conditionals", Conditionals.out),
      ("[GUIDE] Functions", Functions.out),
      ("[TESTS] Casting", Casting.out),
      ("[TESTS] Types and Static Errors", TypesStaticErrors.out),
    ],
  ),
};
