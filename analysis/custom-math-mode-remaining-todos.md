# Custom Math Mode remaining TODOs

The implementation plan is complete as of 2026-07-28. No known release-blocking
work remains for the `sin(x) ** 4` case study.

- Expand the reviewed certificate-adapter catalog beyond the current approved
  rewrite families; keep arbitrary tactic text outside the trust boundary.
- Improve rewrite-authoring ergonomics with catalog search/filtering and clearer
  inline schema errors.
- If profiling finds another long direct semantic precheck, move that bounded
  check into a dedicated worker; proof planning is already incremental and
  cancellable, and Rocq checking already runs in JSCoq's worker.
- Add broader calibrated browser replay coverage as the SVG editor exposes more
  stable automation hooks.
