module TypSolutionMap: (module type of SolutionMap.Make(Solution.TypSolution));

let go: list(Typ.equivalence) => TypSolutionMap.t;
