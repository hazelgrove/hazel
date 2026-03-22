open Util;

type t = ZipperBase.t;

type repair_stats = {
  complete_multitiles: int,
  incomplete_multitiles: int,
  preserved_anchors: int,
};

let empty_repair_stats = {
  complete_multitiles: 0,
  incomplete_multitiles: 0,
  preserved_anchors: 0,
};

let add_repair_stats = (a: repair_stats, b: repair_stats): repair_stats => {
  complete_multitiles: a.complete_multitiles + b.complete_multitiles,
  incomplete_multitiles: a.incomplete_multitiles + b.incomplete_multitiles,
  preserved_anchors: a.preserved_anchors + b.preserved_anchors,
};

let is_multidelimiter_label = (label: Label.t): bool => List.length(label) > 1;

let score_multitile =
    (~anchor_ids: list(Id.t), ~id: Id.t, ~label: Label.t, ~complete: bool)
    : repair_stats =>
  if (!is_multidelimiter_label(label)) {
    empty_repair_stats;
  } else {
    {
      complete_multitiles: complete ? 1 : 0,
      incomplete_multitiles: complete ? 0 : 1,
      preserved_anchors: complete && List.mem(id, anchor_ids) ? 1 : 0,
    };
  };

let rec collect_complete_anchor_ids_segment =
    (acc: list(Id.t), seg: Segment.t): list(Id.t) =>
  List.fold_left(
    (acc, p) =>
      switch (p) {
      | Piece.Tile(t) =>
        let acc =
          List.fold_left(collect_complete_anchor_ids_segment, acc, t.children);
        is_multidelimiter_label(t.label) && Tile.is_complete(t)
          ? [t.id, ...acc] : acc;
      | _ => acc
      },
    acc,
    seg,
  );

let collect_complete_anchor_ids_siblings =
    (acc: list(Id.t), ((pre, suf): Siblings.t)): list(Id.t) => {
  let acc = collect_complete_anchor_ids_segment(acc, pre);
  collect_complete_anchor_ids_segment(acc, suf);
};

let collect_complete_anchor_ids_ancestors =
    (acc: list(Id.t), ancs: Ancestors.t): list(Id.t) =>
  List.fold_left(
    (acc, (a, parent_sibs): Ancestors.generation) => {
      let acc =
        List.fold_left(collect_complete_anchor_ids_segment, acc, fst(a.children));
      let acc =
        List.fold_left(collect_complete_anchor_ids_segment, acc, snd(a.children));
      let total_shards =
        List.length(fst(a.shards)) + List.length(snd(a.shards));
      let acc =
        is_multidelimiter_label(a.label) && total_shards == List.length(a.label)
          ? [a.id, ...acc] : acc;
      collect_complete_anchor_ids_siblings(acc, parent_sibs);
    },
    acc,
    ancs,
  );

let complete_anchor_ids_of_relatives = (rs: Relatives.t): list(Id.t) => {
  let ids =
    collect_complete_anchor_ids_siblings([], rs.siblings)
    |> acc => collect_complete_anchor_ids_ancestors(acc, rs.ancestors);
  List.sort_uniq(compare, ids);
};

let rec repair_stats_of_segment =
    (~anchor_ids: list(Id.t), seg: Segment.t): repair_stats =>
  List.fold_left(
    (acc, p) =>
      switch (p) {
      | Piece.Tile(t) =>
        let self =
          score_multitile(
            ~anchor_ids,
            ~id=t.id,
            ~label=t.label,
            ~complete=Tile.is_complete(t),
          );
        let children =
          List.fold_left(
            (acc, child) =>
              add_repair_stats(acc, repair_stats_of_segment(~anchor_ids, child)),
            empty_repair_stats,
            t.children,
          );
        let acc = add_repair_stats(acc, self);
        add_repair_stats(acc, children)
      | _ => acc
      },
    empty_repair_stats,
    seg,
  );

let repair_stats_of_segments =
    (~anchor_ids: list(Id.t), segs: list(Segment.t)): repair_stats =>
  List.fold_left(
    (acc, seg) =>
      add_repair_stats(acc, repair_stats_of_segment(~anchor_ids, seg)),
    empty_repair_stats,
    segs,
  );

let repair_stats_of_siblings =
    (~anchor_ids: list(Id.t), ((pre, suf): Siblings.t)): repair_stats =>
  add_repair_stats(
    repair_stats_of_segment(~anchor_ids, pre),
    repair_stats_of_segment(~anchor_ids, suf),
  );

let repair_stats_of_ancestors =
    (~anchor_ids: list(Id.t), ancs: Ancestors.t): repair_stats =>
  List.fold_left(
    (acc, (a, parent_sibs): Ancestors.generation) => {
      let total_shards =
        List.length(fst(a.shards)) + List.length(snd(a.shards));
      let self =
        score_multitile(
          ~anchor_ids,
          ~id=a.id,
          ~label=a.label,
          ~complete=total_shards == List.length(a.label),
        );
      let children =
        repair_stats_of_segments(~anchor_ids, fst(a.children) @ snd(a.children));
      let parent_sibs = repair_stats_of_siblings(~anchor_ids, parent_sibs);
      let acc = add_repair_stats(acc, self);
      let acc = add_repair_stats(acc, children);
      add_repair_stats(acc, parent_sibs)
    },
    empty_repair_stats,
    ancs,
  );

let repair_stats_of_relatives =
    (~anchor_ids: list(Id.t), rs: Relatives.t): repair_stats =>
  add_repair_stats(
    repair_stats_of_siblings(~anchor_ids, rs.siblings),
    repair_stats_of_ancestors(~anchor_ids, rs.ancestors),
  );

let should_accept_local_repair =
    (base_stats: repair_stats, candidate_stats: repair_stats): bool =>
  candidate_stats.complete_multitiles > base_stats.complete_multitiles
  || (
    candidate_stats.complete_multitiles == base_stats.complete_multitiles
    && candidate_stats.preserved_anchors == base_stats.preserved_anchors
    && candidate_stats.incomplete_multitiles < base_stats.incomplete_multitiles
  );

let accept_candidate =
    (
      ~base_scope: Relatives.t,
      ~candidate_siblings: Siblings.t,
      ~outer_ancestors: Ancestors.t,
      z: t,
    )
    : t => {
  let anchor_ids = complete_anchor_ids_of_relatives(base_scope);
  let base_stats = repair_stats_of_relatives(~anchor_ids, base_scope);
  let local_relatives =
    {
      Relatives.siblings: candidate_siblings,
      ancestors: [],
    }
    |> Relatives.reassemble;
  let candidate_stats = repair_stats_of_relatives(~anchor_ids, local_relatives);
  if (should_accept_local_repair(base_stats, candidate_stats)) {
    {
      ...z,
      relatives: {
        siblings: local_relatives.siblings,
        ancestors: local_relatives.ancestors @ outer_ancestors,
      },
    };
  } else {
    z;
  };
};
