open Util;

/* PROBE SYSTEM STATE LOCATIONS
 *
 * The probe system maintains state in several places:
 *
 * | State            | Location                       | Scope      | Persisted? |
 * |------------------|--------------------------------|------------|------------|
 * | Manual probes    | Refractors.manuals             | Per-editor | Yes        |
 * | Auto probe IDs   | Refractors.autos.ids           | Per-editor | No         |
 * | Auto ephemerals  | Refractors.autos.ephemerals    | Per-editor | No         |
 * | Auto-def target  | Refractors.auto_def            | Per-editor | No         |
 * | Sample cursor    | Refractors.sample_cursor       | Per-editor | No         |
 * | Display settings | ProbeProj.Settings.s           | Global     | No         |
 * | Window offsets   | ProbeProj.Settings.offset      | Per-probe  | No         |
 * | Sample lengths   | ProbeProj.SampleLength.lengths | Per-sample | No         |
 *
 * Per-editor state is stored in the zipper and flows through model updates.
 * Global/per-probe state in ProbeProj.re uses mutable refs for simplicity.
 *
 * Only `manuals` is persisted (serialized to localStorage/files). Auto probes
 * are recomputed on each edit, and display state is transient. */

/* Simplified entry type for refractors.
 * Unlike projectors, we don't need to store the full ProjectorCore.t:
 * - id is redundant (it's the map key)
 * - syntax is dummy (refractors don't replace syntax, they overlay)
 * When the full Base.projector is needed (for rendering), use to_projector */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type entry = {
  kind: ProjectorCore.Kind.t,
  model: string,
};

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = Id.Map.t(entry);
  let empty = Id.Map.empty;
};

/* Groups auto-probe related state together.
 * - ids: Set-like map (Id.Map.t(unit)) of auto-generated probe IDs
 * - ephemerals: Projector instances for rendering auto probes */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type auto_state = {
  ids: Id.Map.t(unit),
  ephemerals: Map.t,
};

let empty_auto_state = {
  ids: Id.Map.empty,
  ephemerals: Id.Map.empty,
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  manuals: Map.t,
  autos: auto_state,
  sample_cursor: Language.Sample.Cursor.t,
  /* For auto-probe mode: the body ID of the top-level definition
     currently being auto-probed (if any). When the cursor moves to
     a different top-level def, an auto-probe is placed on its body. */
  auto_def: option(Id.t),
};

let init = {
  manuals: Id.Map.empty,
  autos: empty_auto_state,
  sample_cursor: Language.Sample.Cursor.init,
  auto_def: None,
};

let persist = (refractors: t): string =>
  refractors.manuals |> Map.sexp_of_t |> Sexplib.Sexp.to_string;

/* Refractors store a simplified `entry` type in Zipper.Refractor.Map
 * (just kind + model), avoiding redundant id/syntax in serialization.
 * When the full Base.projector is needed for rendering, use `to_projector`. */
let mk_entry = (kind: ProjectorCore.Kind.t): entry => {
  let (module P) = ProjectorInit.to_module(kind);
  let model =
    /* Create dummy syntax just to get the initial model string */
    P.init(Exp(Language.IdTagged.FreshGrammar.Exp.tuple([])))
    |> OptUtil.get_or_fail("Refractor.mk_entry");
  {
    kind,
    model,
  };
};

/* Construct full Base.projector from entry and id, for rendering.
 * Creates dummy syntax with Id.invalid since refractors use skip_inline=true
 * and don't actually display the syntax. */
let to_projector = (id: Id.t, entry: entry): Base.projector =>
  ProjectorCore.mk(
    ~id,
    entry.kind,
    Base.Secondary({
      id: Id.invalid,
      content: Whitespace(""),
    }),
    entry.model,
  );
