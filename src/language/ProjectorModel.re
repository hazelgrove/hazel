open Util;

/* Projector model types.
 *
 * These live here, rather than beside their implementations in
 * haz3lcore/projectors/implementations, because projector models are
 * stored below the modules that consume them: both Base.piece (via
 * ProjectorCore.t) and Grammar.projector_data carry a model, and both
 * sit below the implementations. Defining the union here keeps the
 * dependency static, so `t` can derive show/sexp/yojson/eq normally.
 *
 * Same reason ProjectorKind.t lives here rather than in ProjectorCore.
 *
 * Each implementation aliases its own `model` to the submodule below,
 * e.g. FoldProj has `type model = ProjectorModel.Fold.t`.
 *
 * Adding a projector: add a submodule (or a nullary constructor if it
 * needs no state beyond the syntax), a constructor on `t`, and a case
 * in `kind`. The compiler will point at the dispatch sites in
 * ProjectorInit that need updating. */

module Fold = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    [@default "⋱"]
    text: string,
    expanded: bool,
    always_render: bool,
  };

  let default: t = {
    text: "⋱",
    expanded: false,
    always_render: false,
  };

  /* Shadows the derived decoder so a malformed payload degrades to the
   * default rather than taking down the whole segment decode. The union's
   * derived t_of_sexp picks this up rather than the generated one. */
  let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
    switch (t_of_sexp(sexp)) {
    | exception _ => default
    | t => t
    };
};

module Statics = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t =
    | Expected
    | Self;
};

module Card = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type mode =
    | Show
    | Choose(int)
    | Flipped;

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {mode};

  let default: t = {mode: Show};

  /* See the note on Fold.t_of_sexp */
  let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
    switch (t_of_sexp(sexp)) {
    | exception _ => default
    | m => m
    };
};

module Csv = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t =
    | NoFile
    | FileLoaded({
        filename: string,
        content: string,
        with_headers: bool,
      });
};

module Probe = {
  /* ProbeProj's model wraps RichProbe.packed_model, an existential whose
   * codecs are dispatched through RichProbeRegistry in haz3lcore. Those
   * can't be named from here, so the payload stays a sexp string and
   * ProjectorInit serializes at the boundary. Probe models are a renderer
   * plugin registry with their own codecs already, and this is off the
   * hot path that motivated typing the rest: placeholder and error never
   * decode it. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = string;
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Fold(Fold.t)
  | Probe(Probe.t)
  | Statics(Statics.t)
  | Checkbox
  | Slider
  | SliderF
  | Card(Card.t)
  | Livelit
  | TextArea
  | Table
  | Csv(Csv.t);

let kind = (model: t): ProjectorKind.t =>
  switch (model) {
  | Fold(_) => Fold
  | Probe(_) => Probe
  | Statics(_) => Statics
  | Checkbox => Checkbox
  | Slider => Slider
  | SliderF => SliderF
  | Card(_) => Card
  | Livelit => Livelit
  | TextArea => TextArea
  | Table => Table
  | Csv(_) => Csv
  };

/* Decode a pre-migration model. These were serialized by the old `Cook`
 * functor as an opaque sexp *string*, so the field is always an Atom whose
 * contents parse to the real payload, and the kind came from a sibling
 * field. Callers read that kind off the legacy sexp and pass it here; see
 * ProjectorCore.Legacy.migrate.
 *
 * Anything unparseable falls back to the same defaults the old per-kind
 * decoders used, so a corrupt payload costs one projector's state rather
 * than the whole document. */
let of_legacy_sexp = (kind: ProjectorKind.t, sexp: Sexplib.Sexp.t): t => {
  let payload =
    switch (sexp) {
    | Sexplib.Sexp.Atom(s) =>
      switch (Sexplib.Sexp.of_string(s)) {
      | parsed => parsed
      | exception _ => Sexplib.Sexp.List([])
      }
    | other => other
    };
  switch (kind) {
  | Fold =>
    Fold(
      switch (Fold.t_of_sexp(payload)) {
      | m => m
      | exception _ => Fold.default
      },
    )
  | Statics =>
    Statics(
      switch (Statics.t_of_sexp(payload)) {
      | m => m
      | exception _ => Statics.Expected
      },
    )
  | Card =>
    Card(
      switch (Card.t_of_sexp(payload)) {
      | m => m
      | exception _ => Card.default
      },
    )
  | Csv =>
    Csv(
      switch (Csv.t_of_sexp(payload)) {
      | m => m
      | exception _ => Csv.NoFile
      },
    )
  /* Probe's payload is decoded by RichProbeRegistry up in haz3lcore, so it
   * stays a string; see the Probe submodule above. */
  | Probe =>
    Probe(
      switch (sexp) {
      | Sexplib.Sexp.Atom(s) => s
      | other => Sexplib.Sexp.to_string(other)
      },
    )
  | Checkbox => Checkbox
  | Slider => Slider
  | SliderF => SliderF
  | Livelit => Livelit
  | TextArea => TextArea
  | Table => Table
  };
};
