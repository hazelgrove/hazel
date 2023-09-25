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

/* ID FAQ

   WHAT ARE IDS USED FOR?

   Unique ids are assigned to tiles (and hence, indirectly, to terms)
   at the time of creation of surface syntax. Ids are used as keys in
   various maps (mostly notably the Measured map, which tracks screen
   coordinates for the view, and the Info map which collects static
   data such as type information).

   BUT WHY IS THERE A _LIST_ OF IDS?

   Technically, each tile has a list of ids, to support n-ary forms like
   tuples; there are rep_id functions in Term to canonically extract
   single representative ids from this list where appropriate.

   HOW ARE NEW IDS CREATED?

   In the parts of the implementation which manipulate Zippers, fresh id
   creation is done by threading an IdGen parameter through all of the
   (functions which call) functions where new tiles can be created. This
   generally follows the state monad pattern. When a fresh Id is required,
   the current value of the IdGen is used, and the IdGen is incremented
   and must then be returned to the caller.

   The threading of IdGen through essentially all syntax modification
   functions presents a significant complication for the action code,
   and may eventually be replaced it with a mutable ref.

   WHERE DOES IDGEN LIVE?

   The initial IdGen passed to zipper functions is generally packaged
   along with a zipper through the Zipper.state type. Although in
   principle the initial IdGen could be set by traversing the zipper
   and finding the largest Id, to avoid this traversal we track the
   IdGen along with the zipper state. Each editor mode is responsible
   for this tracking. Ultimately, each zipper action which can result in
   new Ids being created must be sandwiched by calls to
   Editors.get_editor_and_id and Editors.put_editor_and_id, to ensure that
   IdGen state is tracked between actions and properly serialized to
   local storage.

   HOW DO I GENERATE FRESH IDS FOR MY USE CASE?

   Currently there is no easy way to generate fresh IDs in places one
   might concievably want them after Term creation, for example in the
   elaborator or evaluator. Doing so is a significant change with
   indirect implications for architrcture and performance; ask Andrew
   about your use case before attempting this. For some uses, a dummy id
   may be sufficient; this should be documented and use Id.invalid or
   another similar label rather than magic literals. If you do need to
   generated genuinely fresh IDs, then you'll need (A) a strategy
   to route an IdGen to/from your use site to the aformentioned
   Editors functions, and a traversal/mutation strategy within your
   context of use.

   IDS IN DYNAMICS:

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
    |> Util.OptUtil.get(_ => failwith("Uuidm.t_of_sexp: not valid UUID (1)"))
  | _ => failwith("Uuidm.t_of_sexp: not valid UUID (2)");

let yojson_of_t: Uuidm.t => Yojson.Safe.t = t => `String(Uuidm.to_string(t));

let t_of_yojson: Yojson.Safe.t => Uuidm.t =
  fun
  | `String(s) =>
    Uuidm.of_string(s)
    |> Util.OptUtil.get(_ =>
         failwith("Uuidm.t_of_yojson: not valid UUID (1)")
       )
  | _ => failwith("Uuidm.t_of_yojson: not valid UUID (2)");

type t = Uuidm.t;

let mk: unit => t = Uuidm.v4_gen(Random.State.make_self_init());

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
  let init = () => {refs: ref(Map.empty), store: M.new_store()};
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
