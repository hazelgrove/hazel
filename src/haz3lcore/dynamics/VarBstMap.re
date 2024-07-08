open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;
module Sexp = Sexplib.Sexp;

module Inner = {
  include Map.Make(Var);

  /* See IntMap */
  [@deriving (sexp, yojson)]
  type binding('v) = (Var.t, 'v);

  let pp = (pp_v, f, map) =>
    iter((k, v) => Format.fprintf(f, "%s -> %a@\n", k, pp_v, v), map);

  let sexp_of_t = (sexp_of_v: 'v => Sexp.t, map: t('v)): Sexp.t =>
    map |> bindings |> sexp_of_list(sexp_of_binding(sexp_of_v));
  let t_of_sexp = (v_of_sexp: Sexp.t => 'v, sexp: Sexp.t): t('v) =>
    sexp |> list_of_sexp(binding_of_sexp(v_of_sexp)) |> List.to_seq |> of_seq;

  let yojson_of_t =
      (yojson_of_v: 'v => Yojson.Safe.t, map: t('v)): Yojson.Safe.t =>
    map |> bindings |> yojson_of_list(yojson_of_binding(yojson_of_v));
  let t_of_yojson =
      (v_of_yojson: Yojson.Safe.t => 'v, yojson: Yojson.Safe.t): t('v) =>
    yojson
    |> list_of_yojson(binding_of_yojson(v_of_yojson))
    |> List.to_seq
    |> of_seq;
};

module VarBstMap0 = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t_('a) = Inner.t('a);

  let empty = Inner.empty;

  let is_empty = Inner.is_empty;

  let singleton = ((x, a)) => Inner.singleton(x, a);

  let extend = (ctx, (x, a)) => Inner.add(x, a, ctx);

  let update = (ctx, f, x) => Inner.update(x, f, ctx);

  let union = (ctx1, ctx2) =>
    Inner.union((_x, a, _a') => Some(a), ctx1, ctx2);

  let lookup = (ctx, x) => Inner.find_opt(x, ctx);

  let contains = (ctx, x) => Inner.mem(x, ctx);

  let map = f => Inner.mapi((x, a) => f((x, a)));

  let filter = f => Inner.filter((x, a) => f((x, a)));

  let fold = (f, init, ctx) =>
    Inner.fold((x, a, acc) => f((x, a), acc), ctx, init);

  let length = Inner.cardinal;

  let to_list = ctx => ctx |> Inner.to_seq |> List.of_seq;

  let of_list = bindings => bindings |> List.to_seq |> Inner.of_seq;
};

module Ordered = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t_('a) = {
    map: VarBstMap0.t_('a),
    /** The reverse insertion order of bindings (denoted by key). */
    rev_order: list((Var.t, unit)),
  };

  let failwith_keysinconsistent = () =>
    failwith("VarBstMap.Ordered: order key not in map");

  let empty = {map: VarBstMap0.empty, rev_order: []};

  let is_empty = ({map, _}) => VarBstMap0.is_empty(map);

  let singleton = ((x, a)) => {
    {map: VarBstMap0.singleton((x, a)), rev_order: [(x, ())]};
  };

  let extend = ({map, rev_order}, (x, a)) => {
    map: VarBstMap0.extend(map, (x, a)),
    rev_order: [(x, ()), ...List.remove_assoc(x, rev_order)],
  };

  let union =
      (
        {map: map1, rev_order: rev_order1},
        {map: map2, rev_order: rev_order2},
      ) => {
    let rec union_order = (order1, rev_order2) =>
      switch (order1, rev_order2) {
      | ([], order2) => order2
      | ([(x, ()), ...rev_order1'], order2) =>
        let rev_order2' = [(x, ()), ...List.remove_assoc(x, order2)];
        union_order(rev_order1', rev_order2');
      };

    let map = VarBstMap0.union(map1, map2);
    let rev_order = union_order(List.rev(rev_order1), rev_order2);
    {map, rev_order};
  };

  let lookup = ({map, _}, x) => VarBstMap0.lookup(map, x);

  let contains = ({map, _}, x) => VarBstMap0.contains(map, x);

  let mapk = (f, {map, rev_order}) => {
    let map = map |> VarBstMap0.map(f);
    {map, rev_order};
  };

  let mapo = (f, {map, rev_order}) => {
    let map =
      rev_order
      |> List.rev
      |> List.fold_left(
           (map', (x, ())) =>
             switch (VarBstMap0.lookup(map, x)) {
             | Some(a) =>
               let a = f((x, a));
               VarBstMap0.extend(map', (x, a));
             | None => failwith_keysinconsistent()
             },
           VarBstMap0.empty,
         );
    {map, rev_order};
  };

  let filterk = (f, {map, rev_order}) => {
    let map = VarBstMap0.filter(f, map);
    let rev_order =
      rev_order
      |> List.rev
      |> List.filter(((x, _)) => VarBstMap0.contains(map, x))
      |> List.rev;
    {map, rev_order};
  };

  let filtero = (f, {map, rev_order}) => {
    let (map, rev_order) =
      rev_order
      |> List.rev
      |> List.fold_left(
           ((map', rev_order'), (x, ())) => {
             let rev_order' = ref(rev_order');
             let map =
               VarBstMap0.update(
                 map',
                 fun
                 | Some(a) when !f((x, a)) => None
                 | Some(a) => {
                     rev_order' := [(x, ()), ...rev_order'^];
                     Some(a);
                   }
                 | None => failwith_keysinconsistent(),
                 x,
               );
             (map, rev_order'^);
           },
           (map, []),
         );

    {map, rev_order};
  };

  let foldk = (f, init, {map, _}) =>
    VarBstMap0.fold(((x, a), acc) => f((x, a), acc), init, map);

  let foldo = (f, init, {map, rev_order}) =>
    rev_order
    |> List.rev
    |> List.fold_left(
         (acc, (x, ())) =>
           switch (VarBstMap0.lookup(map, x)) {
           | Some(a) => f((x, a), acc)
           | None => failwith_keysinconsistent()
           },
         init,
       );

  let length = ({rev_order, _}) => List.length(rev_order);

  let to_listk = ({map, _}) => VarBstMap0.to_list(map);

  let to_listo = ({map, rev_order}) =>
    rev_order
    |> List.rev
    |> List.map(((x, ())) =>
         switch (VarBstMap0.lookup(map, x)) {
         | Some(a) => (x, a)
         | None => failwith_keysinconsistent()
         }
       );

  let of_list = bindings => {
    let map = VarBstMap0.of_list(bindings);
    let rev_order = bindings |> List.map(((x, _)) => (x, ())) |> List.rev;
    {map, rev_order};
  };

  let without_keys = (keys, m) => {
    filterk(((s, _)) => !List.exists(x => x == s, keys), m);
  };
};

include VarBstMap0;
