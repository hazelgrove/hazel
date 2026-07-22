/* DISPOSAL: disposable migration tooling for the tile FormId change.
 * Delete this file (together with LegacyBase.re, Migrate_slides.re,
 * Migrate_exercises.re, scripts/split_migrate_output.py, and
 * scripts/README_migrate_tile_format.md) once tile-datatype has merged
 * to dev and active feature branches have run the recipe in
 * scripts/README_migrate_tile_format.md. Nothing at runtime depends on
 * it. */

/* LegacyBaseV1: the FormId v1 syntax types — tiles storing a
 * sort-committed form id (Compound(compound_form) |
 * Unsorted(compound_form) | Atom(class, sort, token) |
 * Unmolded(token)) and no sort field — kept so v1-serialized segments
 * still sexp-decode, plus the id-preserving `upgrade` to the current
 * (v2) representation: families + Tok/TokInfix + explicit tile sort.
 * Only the sexp grammar matters here: constructor and field names
 * must match the v1 Base.re/FormId.re exactly (incl. the v1
 * [@sexp.default] shards/children drops and the `(Form ...)` head
 * alias that predated the Compound rename). compound_form and
 * atomic_form still exist unchanged in Form and are reused. Used by
 * the slide migration tool (src/web/Migrate_slides.re); see
 * scripts/README_migrate_tile_format.md. */

open Util;
open Haz3lcore;

module FormV1 = {
  [@deriving sexp]
  type t =
    | Compound(Form.compound_form)
    | Unsorted(Form.compound_form)
    | Atom(Form.atomic_form, Sort.t, Token.t)
    | Unmolded(Token.t);

  /* The earliest v1 sexps spell Compound as `Form`; accept both. */
  let t_of_sexp = {
    let derived = t_of_sexp;
    fun
    | Sexplib.Sexp.List([Sexplib.Sexp.Atom("Form" | "form"), ...args]) =>
      derived(Sexplib.Sexp.List([Sexplib.Sexp.Atom("Compound"), ...args]))
    | s => derived(s);
  };
};

[@deriving sexp]
type segment = list(piece)
and piece =
  | Tile(tile)
  | Grout(Grout.t)
  | Secondary(Secondary.t)
  | Projector(projector)
and tile = {
  id: Id.t,
  form: FormV1.t,
  [@sexp.default [0]] [@sexp_drop_default.sexp]
  shards: list(int),
  [@sexp.default []] [@sexp_drop_default.sexp]
  children: list(segment),
}
and projector = ProjectorCore.t(piece);

let show_mold = (mold: Mold.t): string =>
  Sexplib.Sexp.to_string(Mold.sexp_of_t(mold));

let show_v1 = (f: FormV1.t): string =>
  Sexplib.Sexp.to_string(FormV1.sexp_of_t(f));

/* What the v1 lookup derived for this form: label and mold. Used to
 * assert the upgrade is label/mold-preserving. */
let v1_label = (f: FormV1.t): Label.t =>
  switch (f) {
  | Compound(cf)
  | Unsorted(cf) => Form.get(cf).label
  | Atom(_, _, t)
  | Unmolded(t) => [t]
  };

let v1_mold = (f: FormV1.t): Mold.t =>
  switch (f) {
  | Compound(cf) => Form.get(cf).mold
  | Unsorted(cf) => Form.unmolded_mold(Form.get(cf).label)
  | Atom(a, sort, _) =>
    let (_, molds) = List.assoc(a, Form.atomic_defs);
    switch (List.find_opt((m: Mold.t) => m.out == sort, molds)) {
    | Some(m) => m
    | None => Mold.mk_op(sort, []) /* was not produced by v1 classify */
    };
  | Unmolded(t) => Form.unmolded_mold([t])
  };

/* v1 form => (v2 form, stored sort):
 * - Compound(cf)  => (Compound(family_of(cf)), cf's out sort)
 * - Unsorted(cf)  => (Compound(family_of(cf)), Any) — fallback mold
 * - Atom(IDP,s,t) => (TokInfix(t), s) — the backup-infix shape-role
 * - Atom(_, s, t) => (Tok(t), s)
 * - Unmolded(t)   => (Tok(t), Any) — fallback mold */
let upgrade_form = (f: FormV1.t): (Form.t, Sort.t) => {
  let (form, sort) =
    switch (f) {
    | Compound(cf) => (
        Form.Compound(Form.family_of(cf)),
        Form.get(cf).mold.out,
      )
    | Unsorted(cf) => (Form.Compound(Form.family_of(cf)), Sort.Any)
    | Atom(InfixDelimiterPrefix, s, t) => (Form.TokInfix(t), s)
    | Atom(_, s, t) => (Form.Tok(t), s)
    | Unmolded(t) => (Form.Tok(t), Sort.Any)
    };
  if (Form.label_of(form) != v1_label(f)) {
    failwith(
      Printf.sprintf(
        "LegacyBaseV1.upgrade_form: label not preserved: %s",
        show_v1(f),
      ),
    );
  };
  if (Form.mold_of(form, sort) != v1_mold(f)) {
    failwith(
      Printf.sprintf(
        "LegacyBaseV1.upgrade_form: mold not preserved: %s => mold %s (was %s)",
        show_v1(f),
        show_mold(Form.mold_of(form, sort)),
        show_mold(v1_mold(f)),
      ),
    );
  };
  (form, sort);
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
  let (form, sort) = upgrade_form(t.form);
  {
    id: t.id,
    form,
    sort,
    shards: t.shards,
    children: List.map(upgrade_segment, t.children),
  };
};
