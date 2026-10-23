open Ppx_yojson_conv_lib.Yojson_conv;

/* A Core map in tree form, plus the converters `[@deriving (show, yojson)]`
   expects of a type that contains one.

   Tree form because a map built with Core's `Map.Make` carries its
   comparator, which is made of closures: Marshal (the worker transport)
   rejects it, and polymorphic equality raises on it. A tree holds only keys
   and values, and every accessor is specialised to the key here instead.

   Serialized as a list of (key, value) pairs, sexp and JSON alike. */

module type Key = {
  [@deriving (show, sexp, yojson)]
  type t;
  let compare: (t, t) => int;
};

module Make = (K: Key) => {
  include Map.Make_tree({
    include K;
    include Comparator.Make(K);
  });

  let pp = (pp_v, f, map) =>
    iteri(map, ~f=(~key, ~data) =>
      Format.fprintf(f, "%a -> %a@\n", K.pp, key, pp_v, data)
    );

  [@deriving yojson]
  type binding('v) = (K.t, 'v);

  let yojson_of_t = (yojson_of_v, map) =>
    map |> to_alist |> yojson_of_list(yojson_of_binding(yojson_of_v));
  let t_of_yojson = (v_of_yojson, yojson) =>
    yojson
    |> list_of_yojson(binding_of_yojson(v_of_yojson))
    |> of_alist_reduce(~f=(_, latest) => latest);
};
