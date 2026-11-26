# Canvas layout research trials

The four additional Layout menu choices are opt-in. They use the actual ELK
and WebCoLa implementations, not approximations of those algorithms:

- `elk`: ELK Layered, rightward, layer-sweep crossing reduction and
  network-simplex node placement.
- `elk-down`: the same solver with downward layering.
- `cola`: WebCoLa constrained stress, overlap avoidance, and top-level module
  groups.
- `cola-live`: the same solver with existing group centers as high-weight
  anchors. CoLa's fixed-node constraints permit small settling; this is not an
  exact pin guarantee.

## Adapter boundaries

`CanvasLayoutExperiments.place` makes each host and its terminals one rectangular
layout unit. The returned translation applies to all members together. Existing
Canvas code draws functions, dependency links, module hulls and labels. The
ELK-generated routes are **not** used in this trial. The Circuit option is still
Canvas's own experimental router. CoLa uses top-level module groups, not the full
nested hierarchy. Manual pins and drag offsets apply after automatic placement.

The lazy `canvas-layout-bridge.js` worker computes complete snapshots. The main
thread keeps the previous coherent placement until a result is ready. Its cache
key includes the full node/edge topology, including terminals inside a host group:
adding a function can change an envelope without changing the list of group roots.
Card resizing alone does not launch another solve; Rearrange requests one.

Results for an obsolete topology cannot trigger a repaint of the current one.
Worker failures or an eight-second deadline retain the fallback geometry and
report status in the Layout popover. Rearrange retries. No graph is uploaded.
The engines use a positive world frame. Anchored growth preserves that frame,
except when new geometry would extend above or left of it; then a translation
keeps the graph reachable. Existing terminal geometry can also change as the
host gains functions. These are reasons to inspect recordings, not just the final
layout or the solver's own fixed-node setting.

## Why these families

ELK's direction, port, label and hierarchy support suits diagrams of programs.
CoLa's explicit separation and group constraints suit variable-sized host groups.
A fuller ELK integration would require explicit ports and label sizes and would
consume its routed sections, rather than feeding a placement into the existing
Canvas router. There is no claim that a stress or Fit score identifies the best
reading experience.

Primary sources consulted:

- [ELK Layered documentation](https://eclipse.dev/elk/reference/algorithms/org-eclipse-elk-layered.html)
- [ELK interactive/model constraints](https://eclipse.dev/elk/blog/posts/2023/23-01-09-constraining-the-model.html)
- [WebCoLa](https://ialab.it.monash.edu/webcola/)
- [HySE, 2026](https://onlinelibrary.wiley.com/doi/full/10.1111/cgf.70586)
- [Few Gaps and Few Crossings, 2025](https://arxiv.org/abs/2502.20896)
- [Overview + Detail Compound Layout, 2024](https://arxiv.org/abs/2408.04045)
- [HOLA: Human-like Orthogonal Network Layout](https://pubmed.ncbi.nlm.nih.gov/26390483/)
- [CoRe-GD, ICLR 2024](https://arxiv.org/abs/2402.06706)
- [Evaluating Graph Layout Algorithms, 2024](https://onlinelibrary.wiley.com/doi/10.1111/cgf.15073)
- [Same Quality Metrics, Different Graph Drawings, GD 2025](https://drops.dagstuhl.de/storage/00lipics/lipics-vol357-gd2025/html/LIPIcs.GD.2025.7/LIPIcs.GD.2025.7.html)
- [SIGGRAPH 2025 conference proceedings](https://www.siggraph.org/wp-content/uploads/2025/08/Conference-Papers.html)

HySE, HOLA, CoRe-GD, Few Gaps, and Overview + Detail were reviewed, not implemented.
HySE is a promising mixed-graph candidate, but its unpublished package and unstable
CoSE dependency need a separately pinned trial. The trials are two algorithm
families with four configurations, not four independently novel algorithms.

## Validation

`node test/layout_research.cjs` exercises the actual installed solvers and the
asynchronous bridge. The existing Canvas and Graph layout tests cover host fan
geometry, drag isolation and rendering paths. The local HTML notebook records
five saved programs under all eight new placement/routing combinations, and
three replays of the same Vector Scene trajectory. No API calls are required.
