## Observable Plot MVP Notes

### 1. Reason ↔︎ JavaScript Integration Details

- **Bundling the runtime:** `src/web/www/prebundle.js` imports `@observablehq/plot` and exposes it on `window.Plot`, alongside the existing `window.Algebrite`. This ensures the Plot API is available to `Js_of_ocaml` code.
- **Supported data format:** The projector now owns its `GraphData` module and decoder. Hazel programs should provide either:
  - A tuple whose last element is the series list `(title?, xLabel?, yLabel?, [ (label, [ (x, y), ... ]), ... ])`, or
  - A bare list of `(label, points)` pairs when no metadata is needed.
    Each series label must be a string; each point is a two-element tuple of numbers. The decoder emits warnings for malformed items and computes axis bounds with the same heuristics as the legacy SVG projector.
- **Calling Plot from Reason:** `ObservablePlotProj.re`’s `plot_render` function treats the decoded `GraphData.t` as the source of truth. Points are flattened to `{series, x, y}` rows (`plot_row_to_js` / `plot_rows_to_js_array`) before the projector constructs a Plot configuration.
  - Plot configuration mirrors the original `GraphProj` SVG settings: margins, inner bounds, computed min/max (`plot_inject_float` etc.). This keeps padding and scaling logic consistent while delegating actual drawing to Plot.
  - Plot marks are created through `Js.Unsafe.fun_call(plot_fn("lineY"), ...)`, then composed into a `Plot.plot` call. The resulting DOM node is `appendChild`’d into the projector container after clearing any previous child nodes.
- **Hooks and lifecycle:** The module instantiates `PlotHook = Virtual_dom.Vdom.Attr.Hooks.Make(PlotHookImpl)` (with the input type `GraphData.t`). The hook ensures `plot_render` runs when the element mounts or updates, and tears down (clearing children) on destroy. This mirrors how Bonsai/Vdom hooks are typically used:
  - `PlotHookImpl.init` → clears the container before the element is attached.
  - `PlotHookImpl.on_mount` → executes `plot_render` with the decoded data.
  - `PlotHookImpl.update` → reruns `plot_render` whenever the sample value changes (e.g., probe updates).
  - `PlotHookImpl.destroy` → clears the container to avoid lingering DOM when the projector unmounts.
- **Warning surface:** Rendering still wraps the chart with a warning indicator, so decode issues, missing data, or runtime absence show up identically to the original projector (`build_error_view`). The only new branch is the “runtime unavailable” message when `window.Plot` is missing.
- **Resizable placeholder:** The projector now stores its grid footprint (`width_blocks`/`height_blocks`) in the model, with defaults of 56×12 blocks and minimums of 32×8. A command-drag on the bottom-right handle dispatches resize actions, updating the placeholder and Plot canvas dimensions in lockstep.

### 2. Observable Plot API Design Considerations & Future Work

- **Current scope:** The MVP only exercises `Plot.lineY` with stroke-encoded series, mirroring the existing multi-line chart. Scale domains and labels map 1:1 from `GraphData`, minimizing behavioural drift.
- **Choosing Hook-based rendering:** We avoided embedding Plot-rendered HTML via raw strings or iframes. Using hooks keeps the projector in sync with Bonsai’s virtual DOM lifecycle, making it safe to rerender on sample updates and ensuring cleanup on destroy.
- **Expanding chart coverage:** Observable Plot supports a wide spectrum (bar, scatter, area, faceting, axes, tips, etc.). To expose more of that power inside Hazel:
  1. **Model chart intent:** introduce a Hazel algebraic data type (e.g. `plot_kind = Line of ... | Bar of ... | Scatter of ...`) that captures the subset of Plot configuration we want to support. Each constructor would map to a specific Plot mark invocation.
  2. **Extend decoding:** augment `GraphProj.decode_graph_data` (or create a dedicated decoder) to understand richer inputs—e.g. a tuple containing chart kind + metadata + data arrays. We’ll need robust error reporting to keep the warning UX reliable.
  3. **Rendering dispatch:** switch `plot_render` from a single `Plot.lineY` call to a dispatcher that selects the appropriate mark(s) based on the decoded plot type, potentially layering marks (area + line, scatter + regression, etc.).
  4. **Axis/legend customisation:** capture axis options (log scales, tick formats), interactive affordances (tooltips), and optional channel encodings (color, size) as part of the ADT.
- **Challenges ahead:**
  - Plot’s API is rich; encoding too much of it in Hazel types may get unwieldy. We’ll likely pick a curated subset and iterate.
  - Some features (faceting, composite marks) require nested data shapes or callback behaviour that don’t map cleanly to static data. We might need to enrich Hazel’s runtime representation (e.g. allow custom options as JSON) to stay flexible.
  - Keeping the projector deterministic implies we should avoid features that depend on ambient DOM state (e.g. window sizing) unless we feed those in explicitly.

### 3. Additional Notes & Risks

- **Runtime availability:** The projector checks `typeof Plot !== 'undefined'`. If a user edits the page or we refactor bundling, the projector will fall back to the “runtime unavailable” error. CI should include a bundling step for `prebundle.js` to catch regressions.
- **Performance considerations:** Rendering a medium-sized dataset per sample is fine, but large series could impact responsiveness (Plot renders to SVG). We may later add throttling or caching if probes update at high frequency.
- **Testing strategy:**
  - Unit-level: extend existing projector tests (or add new ones) to simulate decoded graphs with multiple series and ensure `plot_render` handles edge cases (empty series, duplicate labels, invalid numbers).
  - Manual: keep verifying via the projector palette with live runtime data, especially once we introduce multiple chart types.
- **Documentation:** This doc should evolve alongside the Hazel ADT we design for broader Plot coverage, capturing supported chart kinds, expected Hazel syntax, and limitations (e.g. no faceting yet).
