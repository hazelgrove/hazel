/* DISPOSAL: disposable migration tooling for the tile FormId change.
 * Delete this file (together with Migrate_slides.re,
 * Migrate_exercises.re, scripts/split_migrate_output.py, and
 * scripts/README_migrate_tile_format.md) once tile-datatype has merged to dev and active
 * feature branches have run the recipe in
 * scripts/README_migrate_tile_format.md. Nothing at runtime depends on
 * it. */

/* LegacyBase: the pre-FormId Base syntax types (tiles stored label+mold
 * side by side), kept verbatim from the old Base.re so that old serialized
 * segments still sexp-decode, plus the id-preserving `upgrade` to the
 * current Base representation. Only the sexp grammar matters here: type,
 * constructor, and field names must match the old Base.re exactly.
 * Id/Label/Mold/Nib/Grout/Secondary/ProjectorCore are unchanged modules
 * and are reused. Used by the slide migration tool
 * (src/web/Migrate_slides.re); see scripts/README_migrate_tile_format.md. */

open Util;
open Haz3lcore;

[@deriving sexp]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Secondary(Secondary.t)
  | Projector(projector)
and tile = {
  id: Id.t,
  label: Label.t,
  mold: Mold.t,
  shards: list(int),
  children: list(segment),
}
and projector = ProjectorCore.t(piece);

/* Upgrade-path counters for migration reporting:
 * a: exact compound form match (label + mold)
 * b: exact atomic class match (token predicate + registered mold)
 * c: Any-fallback mold => Unsorted/Unmolded (documented mold delta)
 * d: anything else => Form.classify_label (stale-mold tiles) */
let count_compound = ref(0);
let count_atomic = ref(0);
let count_any_fallback = ref(0);
let count_classified = ref(0);
let classified_log: ref(list(string)) = ref([]);

let reset_counts = () => {
  count_compound := 0;
  count_atomic := 0;
  count_any_fallback := 0;
  count_classified := 0;
  classified_log := [];
};

/* The two mold shapes the legacy Molds.get fell back to for labels
 * that fail to mold (see Form.unmolded_mold) */
let any_fallback_molds: list(Mold.t) = [
  Mold.mk_op(Sort.Any, []),
  Mold.mk_bin(Precedence.max, Sort.Any, []),
];

let show_label = (label: Label.t): string => String.concat(" ", label);
let show_mold = (mold: Mold.t): string =>
  Sexplib.Sexp.to_string(Mold.sexp_of_t(mold));

/* Exact reverse lookup (label, mold) => FormId, replicating the
 * priority order of Form's classification tables. `exact` paths must
 * reproduce both label and mold; the Any-fallback paths reproduce the
 * label and the (derived) fallback mold shape. */
let upgrade_form = (label: Label.t, mold: Mold.t): Form.t => {
  let compounds = Form.compound_defs(label);
  let classify = (): Form.t => {
    incr(count_classified);
    let id = Form.classify_label(mold.out, label);
    classified_log :=
      [
        Printf.sprintf(
          "stale-mold tile: label [%s] mold %s => %s",
          show_label(label),
          show_mold(mold),
          Form.show(id),
        ),
        ...classified_log^,
      ];
    id;
  };
  let (id, exact) =
    switch (
      List.find_opt(
        ((_, m): (Form.compound_form, Mold.t)) => m == mold,
        compounds,
      )
    ) {
    | Some((cf, _)) =>
      incr(count_compound);
      (Form.Compound(cf), true);
    | None =>
      let atomic =
        switch (label) {
        | [t] =>
          Form.atomic_candidates(t)
          |> List.find_opt(((_, m): (Form.t, Mold.t)) => m == mold)
        | _ => None
        };
      switch (atomic) {
      | Some((id, _)) =>
        incr(count_atomic);
        (id, true);
      | None when List.mem(mold, any_fallback_molds) =>
        switch (compounds, label) {
        | ([(cf, _), ..._], _) =>
          incr(count_any_fallback);
          (Form.Unsorted(cf), false);
        | ([], [t]) =>
          incr(count_any_fallback);
          (Form.Unmolded(t), false);
        | ([], _) => (classify(), false)
        }
      | None => (classify(), false)
      };
    };
  if (Form.label_of(id) != label) {
    failwith(
      Printf.sprintf(
        "LegacyBase.upgrade_form: label not preserved: [%s] %s => %s",
        show_label(label),
        show_mold(mold),
        Form.show(id),
      ),
    );
  };
  if (exact && Form.mold_of(id) != mold) {
    failwith(
      Printf.sprintf(
        "LegacyBase.upgrade_form: mold not preserved: [%s] %s => %s (mold %s)",
        show_label(label),
        show_mold(mold),
        Form.show(id),
        show_mold(Form.mold_of(id)),
      ),
    );
  };
  id;
};

let rec upgrade_segment = (seg: segment): Base.segment =>
  List.map(upgrade_piece, seg)
and upgrade_piece = (p: piece): Base.piece =>
  switch (p) {
  | Tile(t) => Base.Tile(upgrade_tile(t))
  | Grout(g) => Base.Grout(g)
  | Secondary(s) => Base.Secondary(s)
  | Projector(pr) =>
    Base.Projector({
      id: pr.id,
      kind: pr.kind,
      model: pr.model,
      syntax: upgrade_piece(pr.syntax),
    })
  }
and upgrade_tile = (t: tile): Base.tile => {
  id: t.id,
  form: upgrade_form(t.label, t.mold),
  shards: t.shards,
  children: List.map(upgrade_segment, t.children),
};
