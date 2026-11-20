/* Type blocks: the regions of a program that share a type. Every
   representative expression or pattern whose type has a head (Typ.head,
   after weak-head normalization in the term's own context) is painted with
   it and the blocks are nested by Blocks.nest. */

let of_map: StaticsBase.Map.t => list(Blocks.t);
