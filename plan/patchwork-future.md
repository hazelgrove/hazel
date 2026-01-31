# Patchwork Integration - Future Work

This document tracks planned improvements for the Hazel-Patchwork integration.

For architecture and current implementation documentation, see `docs/patchwork-integration.md`.

---

## Caret Sync Improvements

- [ ] Debounce outgoing caret messages (50ms threshold)
- [ ] Sync selection ranges (highlight what others have selected)
- [x] User name labels next to remote carets (DONE - Jan 2026)
- [ ] Clean up remote caret forwarding in tool.tsx
  - Currently stores all carets in state and re-forwards all on each change
  - Should forward directly in ephemeral message handler (avoid state, avoid N messages per update)

---

## Projector Improvements

- [ ] Consider refractor sync (Probe, Statics) for collaborative debugging sessions
- [ ] Model sync granularity: explore finer-grained CRDT sync for projector models if conflicts become an issue

---

## Performance

- [x] Diff-based sync (IN PROGRESS - Jan 2026)
  - Changed from array to map schema for O(1) Automerge updates
  - Moving to delta protocol (OCaml computes diff, sends only changes)
- [ ] Cache old flat doc to avoid re-conversion on every edit
  - Currently: old_zipper → flat_doc conversion happens on every send
  - Optimization: Store last sent flat_doc in syntax cache, reuse it as "old" state
  - Would eliminate one seg_to_doc call per edit
- [ ] Profile and optimize `FlatConvert` for large documents
- [ ] Consider dirty-tracking instead of full diff
  - Track which pieces changed during edit operation
  - Send O(k) delta where k = changed pieces, not O(n) full document

---

## Code Cleanup

- [ ] Remove excessive profiling logs from tool.tsx (after performance work complete)
- [ ] Update/clarify comments in tool.tsx map handling
- [ ] Investigate src/types.d.ts in patchwork-extra/hazel
  - Appears to be type augmentation for caret messages
  - Possibly redundant with inline type extension in tool.tsx
  - Decide: commit, ignore, or delete

---

## Security

- [ ] Restrict PostMessage origin (currently uses `"*"`)
- [ ] Validate incoming messages more strictly
