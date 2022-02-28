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

module NodeIndex = {
  module NodeMap =
    Map.Make({
      type nonrec t = TagMap.binding(option(t));
      let compare = compare;
    });
  include NodeMap;

  type nonrec t = {
    map: t(int),
    next: int,
  };

  let cardinal = ({map, _}: t): int => map |> NodeMap.cardinal;

  let empty: t = {map: empty, next: 0};

  let find = (node: NodeMap.key, index: t): (int, t) =>
    switch (NodeMap.find_opt(node, index.map)) {
    | None =>
      let map = NodeMap.add(node, index.next, index.map);
      let next = index.next + 1;
      (index.next, {map, next});
    | Some(i) => (i, index)
    };

  let of_list = (nodes: list(NodeMap.key)): t =>
    nodes
    |> List.fold_left((index, node) => index |> find(node) |> snd, empty);
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
  // TODO: elaborate the comments
  | (Sum(Finite(tymapU)), Sum(Finite(tymapV))) =>
    /*
      1. Ensure the sums are of equal cardinality
      2. Build a graph of all possible consistent pairings
      3. Check if the graph has a perfect matching
     */
    let nodesU = TagMap.bindings(tymapU);
    let nodesV = TagMap.bindings(tymapV);
    let n = List.length(nodesU);
    let m = List.length(nodesV);
    n == m
    && (
      // TODO: short circuit if the sorted sum bodies are equal
      // void sums are vacuously consistent
      m == 0
      || {
        let (nodesU, nodesV) =
          ListUtil.combos2(nodesU, nodesV)
          |> List.filter((((tagA, argA_opt), (tagB, argB_opt))) =>
               UHTag.consistent(tagA, tagB)
               && consistent_opt(argA_opt, argB_opt)
             )
          |> List.split;
        let indexU = nodesU |> NodeIndex.of_list;
        let indexV = nodesV |> NodeIndex.of_list;
        NodeIndex.cardinal(indexU) == n
        && NodeIndex.cardinal(indexV) == m
        && {
          let graph =
            BipartiteGraph.of_list(
              List.combine(
                nodesU
                |> List.map(node => NodeIndex.find(node, indexU) |> fst),
                nodesV
                |> List.map(node => [NodeIndex.find(node, indexV) |> fst]),
              ),
            );
          BipartiteGraph.(
            graph |> maximum_cardinality_matching |> M.is_perfect
          );
        };
      }
    );
  // TCSum2
  | (Sum(Elided(tag, ty_opt)), Sum(Elided(tag', ty_opt'))) =>
    !UHTag.equal(tag, tag') || consistent_opt(ty_opt, ty_opt')
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
let matched_finite_sum: t => option(TagMap.t(option(t))) =
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
  switch (UHTag.equal(tag, tag'), ty_opt, ty_opt') {
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
