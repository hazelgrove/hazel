/* Type blocks: the regions of a program that share a type.

   A block is a representative expression or pattern whose type head
   (Typ.head, after weak-head normalization in the term's own context)
   differs from that of its nearest typed ancestor, or that has no typed
   ancestor. A term whose head matches its parent's belongs to the parent's
   block. The level is the number of blocks among a block's ancestors, so the
   root is 0.

   Terms whose type has no head (an unresolved type variable, a module, a
   label, ...) are neither blocks nor parents: their descendants compare
   against the next typed ancestor up. */

/* Every block of the program, painted by its head, in ascending level, so
   painting them in order puts children over parents. */
let of_map: StaticsBase.Map.t => list(Blocks.t);
