# Patchwork Integration - Future Work

This document tracks planned improvements for the Hazel-Patchwork integration.

For architecture and current implementation documentation, see `docs/patchwork-integration.md`.

---

## Caret Sync Improvements

- [ ] Debounce outgoing caret messages (50ms threshold)
- [ ] Sync selection ranges (highlight what others have selected)
- [ ] User name labels next to remote carets

---

## Projector Improvements

- [ ] Consider refractor sync (Probe, Statics) for collaborative debugging sessions
- [ ] Model sync granularity: explore finer-grained CRDT sync for projector models if conflicts become an issue

---

## Performance

- [ ] Consider diff-based sync instead of full-state sync
- [ ] Profile and optimize `FlatConvert` for large documents

---

## Security

- [ ] Restrict PostMessage origin (currently uses `"*"`)
- [ ] Validate incoming messages more strictly
