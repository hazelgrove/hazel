/* ID FAQ

   WHATS AN ID?

   IDs are 128-bit UUIDs (v4 layout); use Id.mk() to generate one. Id.mk
   makes them from a counter plus a random tail fixed per session (see
   below), so they are unique but not individually random.

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
  let byte = Array.init(256, ~f=i => String.make(1, Char.of_int_exn(i)));
  let seed = Random.State.make_self_init();
  let rnd = () => Random.State.int(seed, 256);
  /* byte 8: the variant bits 10, then 6 random bits; bytes 9-15 random */
  let tail = {
    let b8 = byte[0x80 lor rnd() land 0x3f];
    let rest = String.concat(List.init(7, ~f=_ => byte[rnd()]));
    b8 ++ rest;
  };
  let lo = ref(0);
  let hi = ref(0);
  /* Bytes 1 to 15 are the same for 256 ids in a row, so they are made
   * once for each run of 256 (when the counter's bytes above its lowest
   * change), and each id is one concatenation, not eight. The same ids as
   * before. */
  let suffix = ref("");
  let suffix_of = ref(-1);
  () => {
    incr(lo);
    if (lo^ >= 1073741824) {
      lo := 0;
      incr(hi);
      suffix_of := (-1);
    };
    let l = lo^;
    let h = hi^;
    if (l lsr 8 != suffix_of^) {
      suffix_of := l lsr 8;
      suffix :=
        byte[l lsr 8 land 0xff]
        ++ byte[l lsr 16 land 0xff]
        ++ byte[l lsr 24 land 0x3f]
        ++ byte[h land 0xff]
        ++ byte[h lsr 8 land 0xff]
        /* byte 6: the version, 4, then 4 bits of the counter */
        ++ byte[0x40 lor h lsr 16 land 0x0f]
        ++ byte[h lsr 20 land 0xff]
        ++ tail;
    };
    Uuidm.unsafe_of_bytes(byte[l land 0xff] ++ suffix^);
  };
};
let namespace_uuid =
  Uuidm.of_string("6ba7b810-9dad-11d1-80b4-00c04fd430c8")
  |> OptUtil.get(_ => failwith("Invalid namespace UUID"));
let next: t => t = x => Uuidm.v5(namespace_uuid, Uuidm.to_string(x));

let mk_str: string => t = s => Uuidm.v5(namespace_uuid, s);

/* Deterministic derivation from a base id. Distinct salts give
   independent streams that can't collide with each other or with
   `next` chains (different hash preimages). */
let derive = (~salt: string, x: t): t =>
  Uuidm.v5(namespace_uuid, salt ++ ":" ++ Uuidm.to_string(x));

/* An id is a 16-byte string (Uuidm.unsafe_to_bytes is the identity), so
   compare it as one, in the same order as Uuidm.compare. (Uuidm.compare
   already compiles to a string compare, so this is for clarity, not
   speed.) */
let compare: (t, t) => int =
  (a, b) =>
    String.compare(Uuidm.unsafe_to_bytes(a), Uuidm.unsafe_to_bytes(b));
let equal: (t, t) => bool =
  (a, b) =>
    String.equal(Uuidm.unsafe_to_bytes(a), Uuidm.unsafe_to_bytes(b));
let to_string: (~upper: bool=?, t) => string = Uuidm.to_string;
let of_string: (~pos: int=?, string) => option(t) = Uuidm.of_string;

let str3 = (id: t) => id |> to_string |> String.sub(_, ~pos=0, ~len=3);
let str8 = (id: t) => id |> to_string |> String.sub(_, ~pos=0, ~len=8);
let cls = (id: t) => "id" ++ str8(id);

/* OCaml-literal id constructor: pp/show render ids as
 * Haz3lcore.Id.v("<uuid>"), consumed as compilable OCaml in generated
 * exercise/derivation files (and as debug display). */
let v: string => t =
  s =>
    Uuidm.of_string(s)
    |> OptUtil.get(_ => failwith("Id.v: not a valid UUID: " ++ s));
let pp: (Format.formatter, t) => unit =
  (f, id) => Format.fprintf(f, "Haz3lcore.Id.v(\"%s\")", to_string(id));
let show = id => sprintf("Haz3lcore.Id.v(\"%s\")", to_string(id));

module Key = {
  type nonrec t = t;
  let pp = pp;
  let show = show;
  let sexp_of_t = sexp_of_t;
  let t_of_sexp = t_of_sexp;
  let yojson_of_t = yojson_of_t;
  let t_of_yojson = t_of_yojson;
  let compare = compare;
  let hash = Hashtbl.hash;
  include (val Comparator.make(~compare, ~sexp_of_t));
};

/* Tree form for the same reason as MapUtil: ids end up inside values that
   cross the worker boundary by Marshal. Core's tree sets come without
   accessors, so the ones Hazel uses are applied to the comparator here. */
module Set = {
  include Set.Make_tree(Key);

  module T = Core.Set.Tree;
  let mem = (s, id) => T.mem(~comparator=Key.comparator, s, id);
  let add = (s, id) => T.add(~comparator=Key.comparator, s, id);
  let is_subset = (s, ~of_) =>
    T.is_subset(~comparator=Key.comparator, s, ~of_);
  let is_empty = T.is_empty;
  let to_list = T.to_list;
};

module Map = {
  include MapUtil.Make(Key);

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
  "00000000-0000-0000-0000-000000000000" |> Uuidm.of_string |> Option.value_exn;

/* Special id used to denote a trivial (empty) function application */
let nullary_ap_flag: t =
  "DEADBEEF-0000-0000-0000-000000000000" |> Uuidm.of_string |> Option.value_exn;

let is_nullary_ap_flag = (ids: list(t)) =>
  switch (ids) {
  | [id] when Uuidm.equal(id, nullary_ap_flag) => true
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
  let rref = (id, s) => Map.find_exn(s.refs^, id);
  let add = (id, a, s) =>
    switch (Map.find(s.refs^, id)) {
    | None =>
      let r = M.make(s.store, a);
      s.refs := Map.set(s.refs^, ~key=id, ~data=r);
    | Some(_) => ()
    };
  let get = (id, s) => M.get(s.store, M.find(s.store, rref(id, s)));
  let get_opt = (id, s) =>
    Map.find(s.refs^, id) |> Option.map(~f=_ => get(id, s));
  let set = (id, a, s) => M.set(s.store, M.find(s.store, rref(id, s)), a);

  let merge = (f, id, id', s) =>
    ignore(M.merge(s.store, f, rref(id, s), rref(id', s)));
};
