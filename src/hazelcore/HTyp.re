open Sexplib.Std;
open OptUtil.Syntax;

/* types with holes */
[@deriving sexp]
type t =
  | Hole
  | Int
  | Float
  | Bool
  | Arrow(t, t)
  | Sum(sum_body)
  | Prod(list(t))
  | List(t)
and sum_body =
  | Finite(tag_map)
  | Elided(UHTag.t, option(t))
and tag_map = TagMap.t(option(t));

[@deriving sexp]
type join =
  | GLB
  | LUB;

let precedence_Prod = Operators_Typ.precedence(Prod);
let precedence_Arrow = Operators_Typ.precedence(Arrow);
let precedence_const = Operators_Typ.precedence_const;
let precedence = (ty: t): int =>
  switch (ty) {
  | Int
  | Float
  | Bool
  | Hole
  | Prod([])
  | Sum(_)
  | List(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Arrow(_, _) => precedence_Arrow
  };

/* equality
   At the moment, this coincides with default equality,
   but this will change when polymorphic types are implemented */

let rec eq = (ty, ty') =>
  switch (ty, ty') {
  | (Hole, Hole) => true
  | (Hole, _) => false
  | (Int, Int) => true
  | (Int, _) => false
  | (Float, Float) => true
  | (Float, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) => eq(ty1, ty1') && eq(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(Finite(tys)), Sum(Finite(tys'))) =>
    TagMap.equal(
      (map1, map2) =>
        switch (map1, map2) {
        | (None, None) => true
        | (None, _) => false
        | (Some(ty), Some(ty')) => eq(ty, ty')
        | (Some(_), _) => false
        },
      tys,
      tys',
    )
  | (Sum(_), _) => false
  | (Prod(tys1), Prod(tys2)) => ListUtil.equal(eq, tys1, tys2)
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => eq(ty, ty')
  | (List(_), _) => false
  };

module BipartiteGraph = {
  type node = TagMap.binding(option(t));

  module NodeMap =
    Map.Make({
      type t = node;
      let compare = compare;
    });

  module Index = {
    type t = {
      map: NodeMap.t(int),
      next: int,
    };

    let empty: t = {map: NodeMap.empty, next: 0};

    let bindings = ({map, _}: t): list((node, int)) =>
      NodeMap.bindings(map);

    let find = (node: node, index: t): (int, t) =>
      switch (NodeMap.find_opt(node, index.map)) {
      | None =>
        let i = index.next;
        let map = NodeMap.add(node, i, index.map);
        let next = i + 1;
        (i, {map, next});
      | Some(i) => (i, index)
      };
  };

  module IntMap = Map.Make(Int);
  module IntSet = Set.Make(Int);

  module AdjacencyMap = {
    include IntMap;

    type t = IntMap.t(IntSet.t);

    let add = (a: int, b: int, adj: t): t => {
      let insert = (b, bs_opt) =>
        switch (bs_opt) {
        | None => Some(IntSet.singleton(b))
        | Some(bs) => Some(bs |> IntSet.add(b))
        };
      update(a, insert(b), adj);
    };
  };

  module Matching = {
    include IntMap;

    type nonrec t = t(option(int));

    let init = (index: Index.t): t =>
      Index.bindings(index)
      |> List.map(((_, x)) => (x, None))
      |> List.to_seq
      |> of_seq;
  };

  module IntOptionMap =
    Map.Make({
      type t = option(int);
      let compare = compare;
    });

  module DistanceMap = {
    include IntOptionMap;

    type nonrec t = t(int);

    let init = (pairs: Matching.t): t =>
      pairs
      |> Matching.bindings
      |> List.map(((a, b_opt)) =>
           switch (b_opt) {
           | None => (Some(a), 0)
           | Some(_) => (Some(a), Int.max_int)
           }
         )
      |> List.to_seq
      |> of_seq
      |> add(None, Int.max_int);
  };

  module Queue = {
    type t('a) = (list('a), list('a));

    let add = (x: 'a, (front, back): t('a)) => ([x, ...front], back);

    let of_list = (xs: list('a)): t('a) => ([], List.rev(xs));

    let rec take_opt: t('a) => option(('a, t('a))) =
      fun
      | ([], []) => None
      | (front, [x, ...back]) => Some((x, (front, back)))
      | (front, []) => take_opt(([], List.rev(front)));
  };

  // Graph

  type t = {
    indexA: Index.t,
    indexB: Index.t,
    adj: AdjacencyMap.t,
  };

  let empty: t = {
    indexA: Index.empty,
    indexB: Index.empty,
    adj: AdjacencyMap.empty,
  };

  let add = (nodeA: node, nodeB: node, {indexA, indexB, adj}: t): t => {
    let (a, indexA) = indexA |> Index.find(nodeA);
    let (b, indexB) = indexB |> Index.find(nodeB);
    let adj = adj |> AdjacencyMap.add(a, b);
    {indexA, indexB, adj};
  };

  let rec of_list: list((node, node)) => t =
    fun
    | [(a, b), ...tail] => of_list(tail) |> add(a, b)
    | [] => empty;

  // Hopcroft-Karp
  let maximum_perfect_matching = ({indexA, indexB, adj}: t): option(int) => {
    let bfs =
        (dist: DistanceMap.t, pairsB: Matching.t)
        : option((DistanceMap.t, bool)) => {
      let rec loop = (dist, queue) =>
        switch (Queue.take_opt(queue)) {
        | None =>
          dist
          |> DistanceMap.find_opt(None)
          |> Option.map(d0 => (dist, d0 != Int.max_int))
        | Some((a, queue)) =>
          let* da = dist |> DistanceMap.find_opt(Some(a));
          let* d0 = dist |> DistanceMap.find_opt(None);
          let* (dist, queue) =
            !(da < d0)
              ? Some((dist, queue))
              : {
                let* bs = adj |> AdjacencyMap.find_opt(a);
                bs
                |> IntSet.elements
                |> List.fold_left(
                     (acc_opt, b) => {
                       let* (dist, queue) = acc_opt;
                       let* a_opt' = pairsB |> Matching.find_opt(b);
                       let* a' = a_opt';
                       let* da' = dist |> DistanceMap.find_opt(a_opt');
                       !(da' == Int.max_int)
                         ? Some((dist, queue))
                         : {
                           let dist = dist |> DistanceMap.add(a_opt', da + 1);
                           let queue = queue |> Queue.add(a');
                           Some((dist, queue));
                         };
                     },
                     Some((dist, queue)),
                   );
              };
          loop(dist, queue);
        };
      let queue =
        dist
        |> DistanceMap.bindings
        |> List.filter_map(((a_opt, d)) => d == 0 ? a_opt : None)
        |> Queue.of_list;
      loop(dist, queue);
    };

    let rec dfs =
            (
              dist: DistanceMap.t,
              pairsA: Matching.t,
              pairsB: Matching.t,
              a_opt: option(int),
            )
            : option((DistanceMap.t, Matching.t, Matching.t, bool)) =>
      if (Option.is_none(a_opt)) {
        Some((dist, pairsA, pairsB, true));
      } else {
        let* a = a_opt;
        let* da = dist |> DistanceMap.find_opt(a_opt);
        let rec loop =
                (
                  dist: DistanceMap.t,
                  pairsA: Matching.t,
                  pairsB: Matching.t,
                  bs: list(int),
                )
                : option((DistanceMap.t, Matching.t, Matching.t, bool)) => {
          switch (bs) {
          | [b, ...bs'] =>
            let* a_opt' = pairsB |> Matching.find_opt(b);
            let* da' = dist |> DistanceMap.find_opt(a_opt');
            if (da' == da + 1) {
              let* (dist, pairsA, pairsB, result) =
                dfs(dist, pairsA, pairsB, a_opt');
              if (result) {
                let pairsA = pairsA |> Matching.add(b, a_opt);
                let pairsB = pairsB |> Matching.add(a, Some(b));
                Some((dist, pairsA, pairsB, true));
              } else {
                loop(dist, pairsA, pairsB, bs');
              };
            } else {
              loop(dist, pairsA, pairsB, bs');
            };
          | [] =>
            let dist = dist |> DistanceMap.add(a_opt, Int.max_int);
            Some((dist, pairsA, pairsB, false));
          };
        };
        let* bs = adj |> AdjacencyMap.find_opt(a);
        loop(dist, pairsA, pairsB, bs |> IntSet.elements);
      };

    let rec loop =
            (
              dist: DistanceMap.t,
              pairsA: Matching.t,
              pairsB: Matching.t,
              matching: int,
            )
            : option(int) => {
      let* (dist, result) = bfs(dist, pairsB);
      !result
        ? Some(matching)
        : {
          let* (dist, pairsA, pairsB, matching) =
            pairsA
            |> Matching.filter((_, b_opt) => Option.is_none(b_opt))
            |> Matching.bindings
            |> List.map(fst)
            |> List.fold_left(
                 (acc_opt, a) => {
                   let* (dist, pairsA, pairsB, matching) = acc_opt;
                   let+ (dist, pairsA, pairsB, result) =
                     dfs(dist, pairsA, pairsB, Some(a));
                   (dist, pairsA, pairsB, matching + (result ? 1 : 0));
                 },
                 Some((dist, pairsA, pairsB, matching)),
               );
          loop(dist, pairsA, pairsB, matching);
        };
    };

    let pairsA = Matching.init(indexA);
    let pairsB = Matching.init(indexB);
    let dist = DistanceMap.init(pairsA);
    loop(dist, pairsA, pairsB, 0);
  };
};

/* type consistency */

let rec consistent = (x, y) =>
  switch (x, y) {
  | (Hole, _)
  | (_, Hole) => true
  | (Int, Int) => true
  | (Int, _) => false
  | (Float, Float) => true
  | (Float, _) => false
  | (Bool, Bool) => true
  | (Bool, _) => false
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    consistent(ty1, ty1') && consistent(ty2, ty2')
  | (Arrow(_, _), _) => false
  // TCSum1
  | (Sum(Finite(tymapA)), Sum(Finite(tymapB))) =>
    /*
      1. Ensure the sums are of equal cardinality
      2. Build a graph of all possible consistent pairings
      3. Check if the graph has a perfect matching
     */
    let partA = TagMap.bindings(tymapA);
    let partB = TagMap.bindings(tymapB);
    let edges =
      ListUtil.combos2(partA, partB)
      |> List.filter((((tagA, argA_opt), (tagB, argB_opt))) =>
           UHTag.consistent(tagA, tagB) && consistent_opt(argA_opt, argB_opt)
         );
    let n = List.length(partA);
    n == List.length(partB)
    && BipartiteGraph.(of_list(edges) |> maximum_perfect_matching)
    |> Option.map(matching => matching == n)
    |> Option.value(~default=false);

  // TCSum2
  | (Sum(Elided(tag, ty_opt)), Sum(Elided(tag', ty_opt'))) =>
    !UHTag.eq(tag, tag') || consistent_opt(ty_opt, ty_opt')
  // TCSum12
  | (Sum(Finite(tymap)), Sum(Elided(tag, ty_opt)))
  // TCSum21
  | (Sum(Elided(tag, ty_opt)), Sum(Finite(tymap))) =>
    switch (TagMap.find_opt(tag, tymap)) {
    | None => false
    | Some(ty_opt') => consistent_opt(ty_opt, ty_opt')
    }
  | (Sum(_), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.for_all2_opt(consistent, tys1, tys2)
    |> Option.value(~default=false)
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => consistent(ty, ty')
  | (List(_), _) => false
  }

and consistent_opt = (ty_opt: option(t), ty_opt': option(t)): bool =>
  switch (ty_opt, ty_opt') {
  | (None, None) => true
  | (Some(ty), Some(ty')) => consistent(ty, ty')
  | (_, _) => false
  };

let inconsistent = (ty1, ty2) => !consistent(ty1, ty2);

let rec consistent_all = (types: list(t)): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    if (List.exists(inconsistent(hd), tl)) {
      false;
    } else {
      consistent_all(tl);
    }
  };

/* matched arrow types */
let matched_arrow =
  fun
  | Hole => Some((Hole, Hole))
  | Arrow(ty1, ty2) => Some((ty1, ty2))
  | _ => None;

let get_prod_elements: t => list(t) =
  fun
  | Prod(tys) => tys
  | _ as ty => [ty];

let get_prod_arity = ty => ty |> get_prod_elements |> List.length;

/* matched list types */
let matched_list =
  fun
  | Hole => Some(Hole)
  | List(ty) => Some(ty)
  | _ => None;

/* matched sum types */
let matched_sum: t => option(TagMap.t(option(t))) =
  fun
  | Sum(Finite(tymap)) => Some(tymap)
  | Sum(Elided(tag, ty_opt)) => Some(TagMap.singleton(tag, ty_opt))
  | _ => None;

/* complete (i.e. does not have any holes) */
let rec complete =
  fun
  | Hole => false
  | Int => true
  | Float => true
  | Bool => true
  | Arrow(ty1, ty2) => complete(ty1) && complete(ty2)
  | Sum(Finite(tymap)) =>
    tymap
    |> List.map(TupleUtil.bimap(UHTag.is_complete, complete_opt))
    |> List.for_all(TupleUtil.uncurry((&&)))
  | Sum(Elided(tag, ty_opt)) =>
    UHTag.is_complete(tag) && complete_opt(ty_opt)
  | Prod(tys) => tys |> List.for_all(complete)
  | List(ty) => complete(ty)
and complete_opt: option(t) => bool =
  fun
  | None => true
  | Some(ty) => complete(ty);

let rec join = (j, ty1, ty2) =>
  switch (ty1, ty2) {
  | (_, Hole) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty1)
    }
  | (Hole, _) =>
    switch (j) {
    | GLB => Some(Hole)
    | LUB => Some(ty2)
    }
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    switch (join(j, ty1, ty1'), join(j, ty2, ty2')) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Sum(Finite(tymap)), Sum(Finite(tymap'))) =>
    /* if tymap != tymap', then sort them first */
    let (tymap, tymap') =
      TagMap.tags_equal(tymap, tymap')
        ? (tymap, tymap') : (TagMap.sort(tymap), TagMap.sort(tymap'));
    Option.map(
      joined_tys => Sum(joined_tys),
      switch (
        List.fold_left2(
          (acc_opt, (tag, ty_opt), (tag', ty_opt')) =>
            Option.bind(
              acc_opt,
              acc => {
                let+ (tag'', ty_opt'') =
                  join_sum_body_element(j, tag, ty_opt, tag', ty_opt');
                [(tag'', ty_opt''), ...acc];
              },
            ),
          Some([]),
          tymap,
          tymap',
        )
      ) {
      | opt => opt |> Option.map(tymap => Finite(tymap))
      | exception (Invalid_argument(_)) => None
      },
    );
  | (Sum(Elided(tag, ty_opt)), Sum(Elided(tag', ty_opt'))) =>
    let+ (tag'', ty_opt'') =
      join_sum_body_element(j, tag, ty_opt, tag', ty_opt');
    Sum(Elided(tag'', ty_opt''));
  | (Sum(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.map2_opt(join(j), tys1, tys2)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    switch (join(j, ty, ty')) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  }
and join_sum_body_element =
    (
      j: join,
      tag: UHTag.t,
      ty_opt: option(t),
      tag': UHTag.t,
      ty_opt': option(t),
    ) =>
  switch (UHTag.eq(tag, tag'), ty_opt, ty_opt') {
  | (false, _, _)
  | (true, None, None) => Some((tag, None))
  | (true, Some(ty), Some(ty')) =>
    let+ joined_ty = join(j, ty, ty');
    (tag, Some(joined_ty));
  | (_, Some(_), None)
  | (_, None, Some(_)) => None
  };

let join_all = (j: join, types: list(t)): option(t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    if (!consistent_all(types)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => join(j, common_ty, ty)
          },
        Some(hd),
        tl,
      );
    }
  };
};

// TODO: (eric) HTyp.valid always returns true until we have type variables
// let rec valid = (theta: TypVarCtx.t, ty: t): bool =>
//   switch (ty) {
//   | Hole
//   | Int
//   | Float
//   | Bool => true
//   | Arrow(ty1, ty2) => valid(theta, ty1) && valid(theta, ty2)
//   | Sum(tymap) =>
//     tymap
//     |> TagMap.for_all(((_, ty_opt)) =>
//          ty_opt |> Option.map(valid(theta)) |> Option.value(~default=true)
//        )
//   | Prod(tys) => List.for_all(valid(theta), tys)
//   | List(ty) => valid(theta, ty)
//   };
