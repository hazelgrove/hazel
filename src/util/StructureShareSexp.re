/*
     This module adds helpers for creating s-expressions that use some structural sharing
     (https://www.ocamlwiki.com/wiki/Structural_sharing) instead of copying all instances
     of the same record.
 */

let structure_share_map: ref(option(Id.Map.t(Sexplib.Sexp.t))) = ref(None);
let pre_deserialized_map: ref(option(Id.Map.t(Obj.t))) = ref(None);

[@deriving sexp]
type structure_shared = (Sexplib.Sexp.t, Id.Map.t(Sexplib.Sexp.t));

let structure_share_sexp_of_t = (key_f, sexp_of_t, thing: 'a): Sexplib.Sexp.t => {
  switch (structure_share_map^) {
  | Some(m) =>
    let key = key_f(thing);
    if (Id.Map.mem(key, m)) {
      ();
    } else {
      let sexp = sexp_of_t(thing);
      let m = structure_share_map^ |> Option.get;
      structure_share_map := Some(Id.Map.update(key, _ => Some(sexp), m));
    };
    Id.sexp_of_t(key);
  | None => sexp_of_t(thing)
  };
};

let structure_share_t_of_sexp = (t_of_sexp, sexp: Sexplib.Sexp.t): 'a => {
  switch (structure_share_map^) {
  | Some(m) =>
    let id = Id.t_of_sexp(sexp);
    switch (
      Id.Map.find_opt(
        id,
        pre_deserialized_map^ |> Option.value(~default=Id.Map.empty),
      )
    ) {
    | Some(x) =>
      print_endline("REUSED");
      Obj.obj(x);
    | None =>
      switch (Id.Map.find_opt(id, m)) {
      | Some(t) =>
        let v = t_of_sexp(t);
        pre_deserialized_map :=
          Some(
            Id.Map.update(
              id,
              _ => Some(Obj.repr(v)),
              pre_deserialized_map^ |> Option.get,
            ),
          );
        v;
      | None => failwith("structure-sharing deserialization failed")
      }
    };
  | None => t_of_sexp(sexp)
  };
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
      pre_deserialized_map := Some(Id.Map.empty);
      let thing = t_of_sexp(sexp);
      structure_share_map := None;
      pre_deserialized_map := None;
      thing;
    | Some(_) => t_of_sexp(sexp)
    };
  };
  (sexp_of_t', t_of_sexp');
};
