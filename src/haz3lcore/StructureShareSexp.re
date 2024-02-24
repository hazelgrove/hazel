/*
     This module adds helpers for creating s-expressions that use some structural sharing
     (https://www.ocamlwiki.com/wiki/Structural_sharing) instead of copying all instances
     of the same record.
 */

let structure_share_map: ref(option(Id.Map.t(Sexplib.Sexp.t))) = ref(None);

[@deriving sexp]
type structure_shared = (Sexplib.Sexp.t, Id.Map.t(Sexplib.Sexp.t));

// To be used on the data structure where the structure sharing takes place
let structure_share_here =
    (
      key_f: 'a => Id.t,
      sexp_of_t: 'a => Sexplib.Sexp.t,
      t_of_sexp: Sexplib.Sexp.t => 'a,
    )
    : ('a => Sexplib.Sexp.t, Sexplib.Sexp.t => 'a) => {
  let sexp_of_t' = (thing: 'a): Sexplib.Sexp.t => {
    switch (structure_share_map^) {
    | Some(m) =>
      let sexp = sexp_of_t(thing);
      let key = key_f(thing);
      structure_share_map := Some(Id.Map.update(key, _ => Some(sexp), m));
      Id.sexp_of_t(key);
    | None => sexp_of_t(thing)
    };
  };
  let t_of_sexp' = sexp => {
    switch (structure_share_map^) {
    | Some(m) =>
      let id = Id.t_of_sexp(sexp);
      let thing_s =
        switch (Id.Map.find_opt(id, m)) {
        | Some(t) => t
        | None => failwith("structure-sharing deserialization failed")
        };
      t_of_sexp(thing_s);
    | None => t_of_sexp(sexp)
    };
  };
  (sexp_of_t', t_of_sexp');
};

// To be used on the root of the data structure currently being serialized
let structure_share_in = (sexp_of_t, t_of_sexp) => {
  let sexp_of_t' = (thing: 'a): Sexplib.Sexp.t => {
    switch (structure_share_map^) {
    | None =>
      structure_share_map := Some(Id.Map.empty);
      let sexp = sexp_of_t(thing);
      let result: structure_shared = (
        sexp,
        structure_share_map^ |> Option.get,
      );
      structure_share_map := None;
      sexp_of_structure_shared(result);
    | Some(_) => sexp_of_t(thing)
    };
  };

  // To be used only on the root of the data structure currently being serialized
  let t_of_sexp' = (sexp: Sexplib.Sexp.t): 'a => {
    switch (structure_share_map^) {
    | None =>
      let (sexp, map) = structure_shared_of_sexp(sexp);
      structure_share_map := Some(map);
      let thing = t_of_sexp(sexp);
      structure_share_map := None;
      thing;
    | Some(_) => t_of_sexp(sexp)
    };
  };
  (sexp_of_t', t_of_sexp');
};
