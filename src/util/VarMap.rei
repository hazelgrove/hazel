/* Variable bindings keyed by name, as an association list -- the shape the
   typing and evaluation contexts are built on.

   `extend` conses and `lookup` takes the first match, so a name bound twice
   resolves to the most recent binding and the earlier one is shadowed rather
   than replaced. Shadowed bindings stay in the list and reappear in
   `to_list`, which is what makes this a scope stack instead of a map. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t_('a) = list((string, 'a));
let empty: t_('a);
let extend: (t_('a), (string, 'a)) => t_('a);
let lookup: (t_('a), string) => option('a);
let contains: (t_('a), string) => bool;
let filter: (((string, 'a)) => bool, t_('a)) => t_('a);
let to_list: t_('a) => list((string, 'a));
let update: (t_('a), string, 'a => 'a) => t_('a);
