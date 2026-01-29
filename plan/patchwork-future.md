# Patchwork Integration - Future Work

This document tracks planned improvements for the Hazel-Patchwork integration.

For architecture and usage documentation, see `docs/patchwork-integration.md`.

## Caret Sync

Show collaborators' cursor positions in real-time.

- [ ] Add `Caret` message type to protocol (`patchworkmessages.d.ts`)
- [ ] Send caret position updates from Hazel iframe after cursor moves
- [ ] Receive and display remote carets in the editor
- [ ] Handle caret cleanup when collaborators disconnect

Note: `patchwork-extra/hazel/src/types.d.ts` has a `Caret` type stub ready.

## Projector Support

Sync projector/livelit state between collaborators.

- [ ] Extend `FlatTile` or add separate structure for projector placements
- [ ] Include projector state in `HazelDoc`
- [ ] Update `FlatConvert.re` to handle projector serialization
- [ ] Test with various projector types

Currently projectors don't sync - their placements are lost in the flat representation.

## Performance

- [ ] Consider diff-based sync instead of full-state sync
- [ ] Profile and optimize `FlatConvert` for large documents
- [ ] Investigate incremental updates to reduce message size

## Security

- [ ] Restrict PostMessage origin (currently uses `"*"`)
- [ ] Validate incoming messages more strictly
