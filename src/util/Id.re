open Ppx_yojson_conv_lib.Yojson_conv;

/* ID FAQ

   WHATS AN ID?

   IDs are random-generated 128bit UUIDs; use Id.mk() to generate one.

   WHAT ARE IDS USED FOR?

   Unique ids are assigned to tiles (and hence, indirectly, to terms)
   at the time of creation of surface syntax. Ids are used as keys in
   various maps (mostly notably the Measured map, which tracks screen
   coordinates for the view, and the Info map which collects static
   data such as type information). Ids are used for many zipper actions,
   including jump to definition, and are also used to coordinate term
   decorations. Accidentally creating non-unique IDs can be the cause
   of many odd issues for zipper actions and display.

   BUT WHY IS THERE A _LIST_ OF IDS?

   Technically, each tile has a list of ids, to support n-ary forms like
   tuples; there are rep_id functions in Term to canonically extract
   single representative ids from this list where appropriate.

   CAN I USE IDS IN DYNAMICS?

   Currently, DHExps (as produced by the elaborator and produced/consumed
   by the evaluator) do not in general persist ids; the exceptions are
   things like holes and tests which have additional metadata which is
   accumulated duting evaluation. There are many use cases for tracking
   ids more generally during evaluation, but doing so in a principled
   way is a large-scale change with architectural implications.

   */

[@deriving (show({with_path: false}), sexp, yojson)]
let sexp_of_t: Uuidm.t => Sexplib.Sexp.t =
  t => Sexplib.Sexp.Atom(Uuidm.to_string(t));

let t_of_sexp: Sexplib.Sexp.t => Uuidm.t =
  fun
  | Sexplib.Sexp.Atom(s) =>
    Uuidm.of_string(s)
    |> OptUtil.get(_ => failwith("Uuidm.t_of_sexp: not valid UUID (1)"))
  | _ => failwith("Uuidm.t_of_sexp: not valid UUID (2)");

let yojson_of_t: Uuidm.t => Yojson.Safe.t = t => `String(Uuidm.to_string(t));

let t_of_yojson: Yojson.Safe.t => Uuidm.t =
  fun
  | `String(s) =>
    Uuidm.of_string(s)
    |> OptUtil.get(_ => failwith("Uuidm.t_of_yojson: not valid UUID (1)"))
  | _ => failwith("Uuidm.t_of_yojson: not valid UUID (2)");

[@deriving eq]
type t = Uuidm.t;

let mk: unit => t = Uuidm.v4_gen(Random.State.make_self_init());
let namespace_uuid =
  Uuidm.of_string("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
  |> OptUtil.get(_ => failwith("Invalid namespace UUID"));
let next: t => t = x => Uuidm.v5(namespace_uuid, Uuidm.to_string(x));

let mk_str: string => t = s => Uuidm.v5(namespace_uuid, s);

let compare: (t, t) => int = Uuidm.compare;
let to_string: (~upper: bool=?, t) => string = Uuidm.to_string;
let of_string: (~pos: int=?, string) => option(t) = Uuidm.of_string;
let pp: (Format.formatter, t) => unit =
  (f, id) =>
    Format.fprintf(
      f,
      "Option.get(Haz3lcore.Id.of_string(\"%s\"))",
      to_string(id),
    );
let show = id =>
  Format.sprintf(
    "Option.get(Haz3lcore.Id.of_string(\"%s\"))",
    to_string(id),
  );

let str3 = (id: t) => id |> to_string |> String.sub(_, 0, 3);
let str8 = (id: t) => id |> to_string |> String.sub(_, 0, 8);
let cls = (id: t) => "id" ++ str8(id);

/* Reversible UUID transformation functions */
let transform_variant: t => t =
  (id: t) => {
    let uuid_str = to_string(id);
    let len = String.length(uuid_str);
    if (len > 0) {
      let last_char = uuid_str.[len - 1];
      let new_last_char =
        switch (last_char) {
        | '0' .. '8' => Char.chr(Char.code(last_char) + 1)
        | '9' => '0'
        | 'a' .. 'e' => Char.chr(Char.code(last_char) + 1)
        | 'f' => 'a'
        | _ => last_char /* shouldn't happen in valid UUID */
        };
      let new_str =
        String.mapi((i, c) => i == len - 1 ? new_last_char : c, uuid_str);
      of_string(new_str)
      |> OptUtil.get(_ =>
           failwith("transform_variant: invalid UUID generated")
         );
    } else {
      failwith("transform_variant: empty UUID string");
    };
  };

let recover_original: t => t =
  (variant_id: t) => {
    let uuid_str = to_string(variant_id);
    let len = String.length(uuid_str);
    if (len > 0) {
      let last_char = uuid_str.[len - 1];
      let original_last_char =
        switch (last_char) {
        | '1' .. '9' => Char.chr(Char.code(last_char) - 1)
        | '0' => '9'
        | 'b' .. 'f' => Char.chr(Char.code(last_char) - 1)
        | 'a' => 'f'
        | _ => last_char /* shouldn't happen in valid UUID */
        };
      let original_str =
        String.mapi(
          (i, c) => i == len - 1 ? original_last_char : c,
          uuid_str,
        );
      of_string(original_str)
      |> OptUtil.get(_ =>
           failwith("recover_original: invalid UUID generated")
         );
    } else {
      failwith("recover_original: empty UUID string");
    };
  };

[@deriving (sexp, yojson)]
type binding('v) = (t, 'v);

module Map = {
  include Map.Make(Uuidm);

  let sexp_of_t = (sexp_of_v, map) =>
    map |> bindings |> Sexplib.Std.sexp_of_list(sexp_of_binding(sexp_of_v));

  let t_of_sexp = (v_of_sexp, sexp) =>
    sexp
    |> Sexplib.Std.list_of_sexp(binding_of_sexp(v_of_sexp))
    |> List.to_seq
    |> of_seq;

  let yojson_of_t = (yojson_of_v, map) =>
    map |> bindings |> yojson_of_list(yojson_of_binding(yojson_of_v));

  let t_of_yojson = (v_of_yojson, json) =>
    json
    |> list_of_yojson(binding_of_yojson(v_of_yojson))
    |> List.to_seq
    |> of_seq;

  let pp = (pp_v, fmt, map) =>
    bindings(map)
    |> List.iter(((k, v)) =>
         Format.fprintf(fmt, "%a -> %a\n", pp, k, pp_v, v)
       );
};
let invalid: t =
  "00000000-0000-0000-0000-000000000000" |> Uuidm.of_string |> Option.get;

/* Special id used to denote a trivial (empty) function application */
let nullary_ap_flag: t =
  "DEADBEEF-0000-0000-0000-000000000000" |> Uuidm.of_string |> Option.get;

let is_nullary_ap_flag = (ids: list(t)) =>
  switch (ids) {
  | [id] when id == nullary_ap_flag => true
  | _ => false
  };

module Uf: {
  type store('a);
  let init: unit => store(_);
  let add: (t, 'a, store('a)) => unit;
  let get: (t, store('a)) => 'a;
  let get_opt: (t, store('a)) => option('a);
  let set: (t, 'a, store('a)) => unit;
  let merge: (('a, 'a) => 'a, t, t, store('a)) => unit;
} = {
  module M = UnionFind.Make(UnionFind.StoreVector);
  type store('a) = {
    refs: ref(Map.t(M.rref('a))),
    store: M.store('a),
  };
  let init = () => {
    refs: ref(Map.empty),
    store: M.new_store(),
  };
  let rref = (id, s) => Map.find(id, s.refs^);
  let add = (id, a, s) =>
    switch (Map.find_opt(id, s.refs^)) {
    | None =>
      let r = M.make(s.store, a);
      s.refs := Map.add(id, r, s.refs^);
    | Some(_) => ()
    };
  let get = (id, s) => M.get(s.store, M.find(s.store, rref(id, s)));
  let get_opt = (id, s) =>
    Map.find_opt(id, s.refs^) |> Option.map(_ => get(id, s));
  let set = (id, a, s) => M.set(s.store, M.find(s.store, rref(id, s)), a);

  let merge = (f, id, id', s) =>
    ignore(M.merge(s.store, f, rref(id, s), rref(id', s)));
};
