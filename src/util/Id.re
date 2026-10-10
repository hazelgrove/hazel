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

/* mk must be cheap: evaluation mints an id per rebuilt term and per
 * environment binding (hundreds of thousands per run), and drawing each
 * from the OCaml 5 LXM generator costs ~1.5us under js_of_ocaml. Instead:
 * a monotonic counter in the leading bytes (leading so that str8/cls
 * abbreviations stay distinct) plus 62 session-random bits in the tail
 * to keep ids unique across sessions/runtimes, with v4's version and
 * variant bits. An id is Uuidm's 16-byte string, built here by
 * concatenating one-byte strings: under js_of_ocaml a string is a JS
 * string and `++` is JS `+`, where Uuidm.v4's byte buffer was copied and
 * converted on every call (~10% of evaluating a livelit's view). */
let mk: unit => t = {
  let byte = Array.init(256, i => String.make(1, Char.chr(i)));
  let seed = Random.State.make_self_init();
  let rnd = () => Random.State.int(seed, 256);
  /* byte 8: the variant bits 10, then 6 random bits; bytes 9-15 random */
  let tail = {
    let b8 = byte[0x80 lor rnd() land 0x3f];
    let rest = String.concat("", List.init(7, _ => byte[rnd()]));
    b8 ++ rest;
  };
  let lo = ref(0);
  let hi = ref(0);
  () => {
    incr(lo);
    if (lo^ >= 1073741824) {
      lo := 0;
      incr(hi);
    };
    let l = lo^;
    let h = hi^;
    Uuidm.unsafe_of_bytes(
      byte[l land 0xff]
      ++ byte[l lsr 8 land 0xff]
      ++ byte[l lsr 16 land 0xff]
      ++ byte[l lsr 24 land 0x3f]
      ++ byte[h land 0xff]
      ++ byte[h lsr 8 land 0xff]
      /* byte 6: the version, 4, then 4 bits of the counter */
      ++ byte[0x40 lor h lsr 16 land 0x0f]
      ++ byte[h lsr 20 land 0xff]
      ++ tail,
    );
  };
};
let namespace_uuid =
  Uuidm.of_string("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
  |> OptUtil.get(_ => failwith("Invalid namespace UUID"));
let next: t => t = x => Uuidm.v5(namespace_uuid, Uuidm.to_string(x));

let mk_str: string => t = s => Uuidm.v5(namespace_uuid, s);

/* An id is a 16-byte string (Uuidm.unsafe_to_bytes is the identity), so
   compare it as one: the same order as Uuidm.compare, which is Stdlib's
   polymorphic compare, without its generic walk. Every Id.Map lookup
   compares ids. */
let compare: (t, t) => int =
  (a, b) =>
    String.compare(Uuidm.unsafe_to_bytes(a), Uuidm.unsafe_to_bytes(b));
let equal: (t, t) => bool =
  (a, b) =>
    String.equal(Uuidm.unsafe_to_bytes(a), Uuidm.unsafe_to_bytes(b));
let to_string: (~upper: bool=?, t) => string = Uuidm.to_string;
let of_string: (~pos: int=?, string) => option(t) = Uuidm.of_string;

let str3 = (id: t) => id |> to_string |> String.sub(_, 0, 3);
let str8 = (id: t) => id |> to_string |> String.sub(_, 0, 8);
let cls = (id: t) => "id" ++ str8(id);

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

[@deriving (sexp, yojson)]
type binding('v) = (t, 'v);

module Ord = {
  type nonrec t = t;
  let compare = compare;
};

module Set = Set.Make(Ord);

module Map = {
  include Map.Make(Ord);

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

  /* Outputs valid OCaml code for empty maps only.
   * Non-empty maps will fail - this is intentional. Currently only empty maps
   * are serialized via show (see Refractors.for_serialization which resets
   * multis to empty before serialization). If you need to persist non-empty
   * Id.Map values, implement proper nested Id.Map.add output here. */
  let pp = (_pp_v, fmt, map) =>
    if (is_empty(map)) {
      Format.fprintf(fmt, "Haz3lcore.Id.Map.empty");
    } else {
      failwith(
        "Id.Map.pp: non-empty map serialization not implemented. "
        ++ "See Refractors.for_serialization if you're seeing this during exercise export.",
      );
    };
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
