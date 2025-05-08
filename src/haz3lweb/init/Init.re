open Haz3lcore;

let startup: PersistentData.t = {
  scratch: (0, List.init(8, _ => Zipper.init() |> PersistentZipper.persist)),
  documentation: (0, [("Casting", Casting.out)]),
};
