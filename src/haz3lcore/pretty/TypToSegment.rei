/* Rendering a type, and naming the tokens of that render.

   A type is first put into the form its rendering takes -- Sig desugared to
   labeled tuples, parens inserted as real nodes, every node carrying the ids
   PadIds says its rendering consumes, each naming one node. Preparing adds
   nodes, and those nodes carry tokens, so the ids naming a rendered token
   belong to the prepared form and nothing outside this module can hold one. */

open Language;

let typ_to_segment:
  (~settings: ExpToSegment.Settings.t, Typ.t) => Base.segment;

/* Render [typ], and report which of its rendered tokens [against] does not
   account for. The segment is the one render those ids describe: preparing
   mints fresh paren ids, so re-rendering would not answer to them. */
let typ_to_segment_with_diff_ids:
  (
    ~settings: ExpToSegment.Settings.t,
    ~ctx: Ctx.t=?,
    ~against: Typ.t,
    Typ.t
  ) =>
  (Base.segment, Id.Set.t);

/* Whether preparing [typ] gives every node the ids its rendering consumes,
   so the renderer never mints one. Preparing guarantees this; exposed so it
   can be checked over generated types. */
let ids_sufficient: (~settings: ExpToSegment.Settings.t, Typ.t) => bool;
