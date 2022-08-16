open Sexplib.Std;
module Sexp = Sexplib.Sexp;

module Inner = {
  include Map.Make(Var);

  /* See IntMap */
  [@deriving sexp]
  type binding('v) = (Var.t, 'v);

  let sexp_of_t = (sexp_of_v: 'v => Sexp.t, map: t('v)): Sexp.t =>
    map |> bindings |> sexp_of_list(sexp_of_binding(sexp_of_v));
  let t_of_sexp = (v_of_sexp: Sexp.t => 'v, sexp: Sexp.t): t('v) =>
    sexp |> list_of_sexp(binding_of_sexp(v_of_sexp)) |> List.to_seq |> of_seq;
};

module VarBstMap0 = {
  [@deriving sexp]
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
  [@deriving sexp]
  type t_('a) = {
    map: VarBstMap0.t_('a),
    order: list((Var.t, unit)),
  };

  let failwith_keysinconsistent = () =>
    failwith("VarBstMap.Ordered: order key not in map");

  let empty = {map: VarBstMap0.empty, order: []};

  let is_empty = ({map, _}) => VarBstMap0.is_empty(map);

  let singleton = ((x, a)) => {
    {map: VarBstMap0.singleton((x, a)), order: [(x, ())]};
  };

  let extend = ({map, order}, (x, a)) => {
    map: VarBstMap0.extend(map, (x, a)),
    order: [(x, ()), ...List.remove_assoc(x, order)],
  };

  let union = ({map: map1, order: order1}, {map: map2, order: order2}) => {
    let rec union_order = (rev_order1, order2) =>
      switch (rev_order1, order2) {
      | ([], order2) => order2
      | ([(x, a), ...rev_order1], order2) =>
        let order2' = [(x, a), ...List.remove_assoc(x, order2)];
        union_order(rev_order1, order2');
      };

    let map = VarBstMap0.union(map1, map2);
    let order = union_order(List.rev(order1), order2);
    {map, order};
  };

  let lookup = ({map, _}, x) => VarBstMap0.lookup(map, x);

  let contains = ({map, _}, x) => VarBstMap0.contains(map, x);

  let mapk = (f, {map, order}) => {
    let map = map |> VarBstMap0.map(f);
    {map, order};
  };

  let mapo = (f, {map, order}) => {
    let map =
      order
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
    {map, order};
  };

  let filterk = (f, {map, order}) => {
    let map = VarBstMap0.filter(f, map);
    let order =
      order
      |> List.rev
      |> List.filter(((x, _)) => VarBstMap0.contains(map, x))
      |> List.rev;
    {map, order};
  };

  let filtero = (f, {map, order}) => {
    let (map, order_rev) =
      order
      |> List.rev
      |> List.fold_left(
           ((map, order_rev'), (x, ())) => {
             let order_rev' = ref(order_rev');
             let map =
               VarBstMap0.update(
                 map,
                 fun
                 | Some(a) when !f((x, a)) => None
                 | Some(a) => {
                     order_rev' := [(x, ()), ...order_rev'^];
                     Some(a);
                   }
                 | None => failwith_keysinconsistent(),
                 x,
               );
             (map, order_rev'^);
           },
           (map, order),
         );
    let order = order_rev |> List.rev;

    {map, order};
  };

  let foldk = (f, init, {map, _}) =>
    VarBstMap0.fold(((x, a), acc) => f((x, a), acc), init, map);

  let foldo = (f, init, {map, order}) =>
    order
    |> List.rev
    |> List.fold_left(
         (acc, (x, ())) =>
           switch (VarBstMap0.lookup(map, x)) {
           | Some(a) => f((x, a), acc)
           | None => failwith_keysinconsistent()
           },
         init,
       );

  let length = ({order, _}) => List.length(order);

  let to_listk = ({map, _}) => VarBstMap0.to_list(map);

  let to_listo = ({map, order}) =>
    order
    |> List.rev
    |> List.map(((x, ())) =>
         switch (VarBstMap0.lookup(map, x)) {
         | Some(a) => (x, a)
         | None => failwith_keysinconsistent()
         }
       );

  let of_list = bindings => {
    let map = VarBstMap0.of_list(bindings);
    let order = bindings |> List.map(((x, _)) => (x, ())) |> List.rev;
    {map, order};
  };
};

include VarBstMap0;
