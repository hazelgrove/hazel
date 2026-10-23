/* S-expression serialization that writes each shared, `Id`-keyed value once
   and refers to it by id elsewhere, instead of copying it at every
   occurrence (https://www.ocamlwiki.com/wiki/Structural_sharing).

   Wrap the root type's converters with `structure_share_in`; inside it, the
   converters of any type that should be shared go through
   `structure_share_sexp_of_t`/`structure_share_t_of_sexp`. Outside a
   `structure_share_in` those two are the plain converters. The sharing table
   is module-level state, so one serialization at a time. */

[@deriving sexp]
type structure_shared = (Sexplib0.Sexp.t, Id.Map.t(Sexplib0.Sexp.t));
/** Takes the id function, then the plain converter. */
let structure_share_sexp_of_t:
  ('a => Id.t, 'a => Sexplib0.Sexp.t, 'a) => Sexplib0.Sexp.t;
let structure_share_t_of_sexp: (Sexplib0.Sexp.t => 'a, Sexplib0.Sexp.t) => 'a;
let structure_share_in:
  ('a => Sexplib0.Sexp.t, Sexplib0.Sexp.t => 'a) =>
  ('a => Sexplib0.Sexp.t, Sexplib0.Sexp.t => 'a);
