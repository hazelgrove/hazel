open Util_web;

/* PROBE SYSTEM STATE LOCATIONS
 *
 * The probe system maintains state in several places:
 *
 * | State            | Location                       | Scope      | Persisted? |
 * |------------------|--------------------------------|------------|------------|
 * | Manual probes    | Refractors.manuals             | Per-editor | Yes        |
 * | Multi probe IDs  | Refractors.multis.ids           | Per-editor | No         |
 * | Multi ephemerals | Refractors.multis.ephemerals    | Per-editor | No         |
 * | Multi suppressed | Refractors.multis.suppressed    | Per-editor | No         |
 * | Auto probe target| Refractors.autoprobe_target    | Per-editor | No         |
 * | Sample focus    | Refractors.sample_focus       | Per-editor | No         |
 * | Display settings | ProbeProj.Settings.s           | Global     | No         |
 * | Window offsets   | ProbeProj.Settings.offset      | Per-probe  | No         |
 * | Sample lengths   | ProbeProj.SampleLength.lengths | Per-sample | No         |
 *
 * Per-editor state is stored in the zipper and flows through model updates.
 * Global/per-probe state in ProbeProj.re uses mutable refs for simplicity.
 *
 * Only `manuals` is persisted (serialized to localStorage/files). Multi probes
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
};

module RefractorList = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = list((Id.t, entry));
};

/* Groups multi-probe related state together.
 * - ids: Set-like map (Id.Map.t(unit)) of multi-probe IDs
 * - ephemerals: Projector instances for rendering multi probes */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type multi_state = {
  ids: Id.Map.t(unit),
  suppressed: Id.Map.t(unit),
  ephemerals: Map.t,
};

let empty_multi_state = {
  ids: Id.Map.empty,
  suppressed: Id.Map.empty,
  ephemerals: Id.Map.empty,
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t = {
  manuals: RefractorList.t,
  multis: multi_state,
  sample_focus: Language.Sample.Focus.t,
  /* For auto probe: the body ID of the top-level definition currently
     being probed (if any). When the cursor moves to a different top-level
     def, a multi probe is placed on its body. */
  autoprobe_target: option(Id.t),
  /* When a probe is added, this stores the target IDs (in lexical order)
     so that when evaluation results return, we can set the sample focus
     to the first sample of the first probe that has samples. */
  pending_probe_cursor: option(list(Id.t)),
};

let init = {
  manuals: [],
  multis: empty_multi_state,
  sample_focus: Language.Sample.Focus.init,
  autoprobe_target: None,
  pending_probe_cursor: None,
};

let persist = (refractors: t): string =>
  refractors.manuals |> RefractorList.sexp_of_t |> Sexplib.Sexp.to_string;

/* Prepares refractors for serialization by resetting non-persistable state.
 * Only `manuals` is persisted - see state location docs at top of file.
 * Used by both sexp serialization (persist) and show serialization (Exercise export). */
let for_serialization = (refractors: t): t => {
  ...init,
  manuals: refractors.manuals,
};

/* Refractors store a simplified `entry` type in Zipper.Refractor.Map
 * (just kind + model), avoiding redundant id/syntax in serialization.
 * When the full Base.projector is needed for rendering, use `to_projector`. */
let mk_entry = (~model=?, kind: ProjectorCore.Kind.t): entry => {
  let (module P) = ProjectorInit.to_module(kind);
  let model =
    model
    |> OptUtil.get(
         () => {
           /* Create dummy syntax just to get the initial model string */
           P.init(Exp(Language.IdTagged.FreshGrammar.Exp.tuple([])))
           |> OptUtil.get_or_fail("Refractor.mk_entry")
         },
         _,
       );
  {
    kind,
    model,
  };
};

/* Construct full Base.projector from entry and id, for rendering.
 * Takes the actual syntax segment so projectors can access the
 * underlying term (needed for syntax rewriting in rich probes). */
let to_projector =
    (syntax: Base.piece, id: Id.t, entry: entry): Base.projector =>
  ProjectorCore.mk(~id, entry.kind, syntax, entry.model);
