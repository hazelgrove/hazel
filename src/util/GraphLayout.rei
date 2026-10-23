/* GraphLayout — a small, deterministic, layered graph-layout engine.

   Pure geometry: no rendering, no app types. Callers describe a graph of
   circular nodes plus "attachments" (nodes placed relative to a host
   rather than ranked — satellites, badges, formers) and get back
   positions. The API is deliberately close to what industrial engines
   (dagre / ELK) consume, so one could be dropped in behind it later.

   Guarantees:
   - Deterministic: output depends only on the spec (input order breaks
     ties); no randomness, no wall-clock.
   - Layered left-to-right: `ranked` edges constrain src to a strictly
     earlier column than dst (cycles are guarded, not an error).
   - Crossing reduction: barycenter ordering sweeps within columns.
   - Spacing derives from node radii + gaps; attachments are placed by
     collision-aware ring search around their host and never overlap
     already-placed nodes (up to the search's ring limit). */

type pos = {
  x: float,
  y: float,
};

module Spec: {
  type node = {
    id: string,
    radius: float,
    /* extra reserved vertical clearance beyond attachments (e.g. an
       attached label legend); spacing and relief treat it as halo */
    extent_above: float,
    extent_below: float,
  };

  /* ranked: constrains column order (src strictly left of dst) AND
     informs crossing reduction. Unranked edges inform ordering only. */
  type edge = {
    src: string,
    dst: string,
    ranked: bool,
  };

  /* Preferred placement side for an attachment, relative to its host. */
  type side =
    | In /* left */
    | Out /* right */
    | Above
    | AboveLeft /* diagonal up-left; keeps the column above the host free */
    | Below;

  /* An attachment is placed near `host` (which may itself be an
     attachment — chains resolve in passes) at rim-to-center distance
     `dist`, starting from the preferred side and ring-searching for a
     collision-free slot. */
  type attachment = {
    id: string,
    host: string,
    radius: float,
    prefer: side,
    dist: float,
  };

  type t = {
    nodes: list(node),
    edges: list(edge),
    attachments: list(attachment),
    col_gap: float, /* min horizontal gap between column rims */
    row_gap: float, /* min vertical gap between rims within a column */
    margin: float, /* top-left padding */
    x_stretch: float, /* multiplies column x-centers (panel fitting) */
    y_stretch: float, /* multiplies row y-centers (pane-height filling) */
    order_sweeps: int, /* barycenter iterations (0 = input order) */
    /* damped ordering: a node passes a neighbor only when its barycenter
       beats the neighbor's by more than this many row positions; ties
       keep the current (initially input/program) order. 0 = plain sort. */
    order_hysteresis: float,
  };

  /* Sensible defaults; override fields as needed. */
  let default: t;
};

type result = {
  positions: list((string, pos)),
  /* column index per ranked node (attachments absent) */
  ranks: list((string, int)),
};

let layout: Spec.t => result;

let pos_of: (result, string) => option(pos);
