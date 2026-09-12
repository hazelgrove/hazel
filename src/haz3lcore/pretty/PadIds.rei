/* How many ids printing a term consumes, and giving a term that many.

   ExpToSegment mints an id wherever a piece it emits needs one the term does
   not carry. An id minted there is in the segment but in no term, so nothing
   downstream can name the piece it labels -- which is how a type probe comes
   to leave a token uncoloured. Padding a term here instead keeps every piece
   nameable. */

open Language;

/* [ids] cut or extended to exactly [n], with fresh ids for any shortfall.
   The result has no duplicates within itself and none equal to a [forbidden]
   id, so the pieces it names cannot collide. */
let pad_ids: (~forbidden: list(Id.t)=?, int, list(Id.t)) => list(Id.t);

/* How many ids printing this type consumes. ExpToSegment.typ_to_pretty pads
   from this rather than from a count of its own, so the printer and anything
   preparing ids for it cannot disagree. */
let necessary_ids: Typ.t => int;

/* Give every node the ids printing it consumes, so the printer uses these
   rather than minting its own. */
let pad_typ_ids: Typ.t => Typ.t;
