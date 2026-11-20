/* Evaluation blocks: the regions of a program by how often they were
   evaluated, a heat map. Every expression the last evaluation reached is
   painted with the heat of its count (Blocks.heat) and the blocks are
   nested by Blocks.nest, so a block appears where the heat changes: a
   function body called more often than its surroundings, a branch taken
   less often. Expressions never reached, and the counts of ids the program
   no longer has, paint nothing. */

let of_evaluations:
  (~info_map: StaticsBase.Map.t, Id.Map.t(int)) => list(Blocks.t);
