/* Printing a type as a segment, and naming the tokens it printed as.

   A type is first put into the form it prints in -- Sig desugared to labeled
   tuples, parens inserted as real nodes, every node carrying the ids PadIds
   says printing it consumes, each naming one node. Preparing adds nodes that
   carry tokens of their own, so the ids here name that form, not the type
   passed in. */

open Language;

let typ_to_segment:
  (~settings: ExpToSegment.Settings.t, Typ.t) => Base.segment;

/* Print [typ], and report the id of each token it printed for a type
   variable, by name: first each use, then each binder. The segment is the
   one those ids name. */
let typ_to_segment_with_var_ids:
  (~settings: ExpToSegment.Settings.t, Typ.t) =>
  (Base.segment, list((string, Id.t)), list((string, Id.t)));

/* Print [typ], and report which of its tokens [against] does not account
   for. The segment is the one printing those ids describe: preparing mints
   fresh paren ids, so printing again would not answer to them. */
let typ_to_segment_with_diff_ids:
  (
    ~settings: ExpToSegment.Settings.t,
    ~ctx: Ctx.t=?,
    ~against: Typ.t,
    Typ.t
  ) =>
  (Base.segment, Id.Set.t);

/* Whether preparing [typ] gives every node the ids printing it consumes, so
   the printer never mints one. Preparing guarantees this. */
let ids_sufficient: (~settings: ExpToSegment.Settings.t, Typ.t) => bool;
