# Patchwork Sync Profiling Results

Profiling runs from Jan 2026 after implementing delta-based sync protocol.

Test setup: ~1264 pieces in document, two browser windows (sender/receiver).

---

## Run 1: Single Character Insertion (First Action After Reload)

Note: First action has warmup overhead (Automerge/IndexedDB/WASM initialization).

### Sender

```
[PERF] seg_to_doc traversed 1264 pieces
[PERF] old_seg_to_doc: 4.00ms
[PERF] seg_to_doc traversed 1264 pieces
[PERF] seg_to_doc (1264 pieces): 3.00ms
[PERF] compute_delta: 3.00ms
[PERF] Delta: 1 changed, 0 added, 0 deleted
[PERF] js_of_state: 0.00ms
[PERF] Payload size: 302 bytes (0.29 KB)
[PERF] postMessage_send: 0.00ms
[PERF] send_to_parent: 4.00ms
[PERF] send_state_total: 12.00ms
[PERF] Received state from iframe at 52148.00ms (280 bytes / 0.27 KB)
[PERF] Received 1 pieces as map (no conversion needed)
[PERF] doesStateEqualDoc check: 1.00ms (equal=false)
[PERF] UUID-based diff took: 0.00ms (changed: 1, added: 0)
[PERF] handle.change() MAP UPDATES took: 169.00ms (changed: 1, added: 0)
[PERF] Total state processing: 171.00ms
[PERF] Skipping echo - change originated from iframe
```

### Receiver

```
[PERF] Doc changed: 1 affected pieces, titleChanged=false
[PERF] Sending delta to iframe: 1 pieces, 280 bytes (0.27 KB)
[PERF] Total send to iframe took: 3.10ms
[PERF] flatdoc_of_hazeldoc: 0.10ms
[PERF] Received delta with 1 pieces
[PERF] receive_state_total: 0.30ms
[PERF] zip_current: 0.00ms
[PERF] seg_to_doc traversed 1264 pieces
[PERF] seg_to_doc_current: 2.40ms
[PERF] merge_docs: 0.00ms
[PERF] Merged doc has 1265 pieces
[PERF] doc_to_seg reconstructed 1264 pieces
[PERF] doc_to_seg_merged: 0.60ms
[PERF] unzip_segment (1264 pieces): 0.00ms
[PERF] cursor_repositioning: 33.60ms
[PERF] sync_replace_total: 38.50ms
```

---

## Run 2: Single Character Insertion (Steady State)

Second action - no warmup overhead.

### Sender

```
[PERF] seg_to_doc traversed 1264 pieces
[PERF] old_seg_to_doc: 3.00ms
[PERF] seg_to_doc traversed 1264 pieces
[PERF] seg_to_doc (1264 pieces): 2.00ms
[PERF] compute_delta: 3.00ms
[PERF] Delta: 1 changed, 0 added, 0 deleted
[PERF] js_of_state: 0.00ms
[PERF] Payload size: 307 bytes (0.30 KB)
[PERF] postMessage_send: 0.00ms
[PERF] send_to_parent: 4.00ms
[PERF] Received state from iframe at 282052.00ms (285 bytes / 0.28 KB)
[PERF] send_state_total: 11.00ms
[PERF] Received 1 pieces as map (no conversion needed)
[PERF] doesStateEqualDoc check: 3.00ms (equal=false)
[PERF] UUID-based diff took: 0.00ms (changed: 1, added: 0)
[PERF] handle.change() MAP UPDATES took: 29.00ms (changed: 1, added: 0)
[PERF] Total state processing: 32.00ms
[PERF] Skipping echo - change originated from iframe
```

### Receiver

```
[PERF] Doc changed: 1 affected pieces, titleChanged=false
[PERF] Sending delta to iframe: 1 pieces, 285 bytes (0.28 KB)
[PERF] Total send to iframe took: 3.60ms
[PERF] flatdoc_of_hazeldoc: 0.10ms
[PERF] Received delta with 1 pieces
[PERF] receive_state_total: 0.30ms
[PERF] zip_current: 0.00ms
[PERF] seg_to_doc traversed 1264 pieces
[PERF] seg_to_doc_current: 1.80ms
[PERF] merge_docs: 0.00ms
[PERF] Merged doc has 1265 pieces
[PERF] doc_to_seg reconstructed 1264 pieces
[PERF] doc_to_seg_merged: 0.60ms
[PERF] unzip_segment (1264 pieces): 0.00ms
[PERF] cursor_repositioning: 32.30ms
[PERF] sync_replace_total: 36.60ms
```

---

## Run 3: Large Paste (417 New Pieces)

### Sender

```
[PERF] seg_to_doc traversed 1267 pieces
[PERF] old_seg_to_doc: 4.00ms
[PERF] seg_to_doc traversed 1684 pieces
[PERF] seg_to_doc (1684 pieces): 1.00ms
[PERF] compute_delta: 3.00ms
[PERF] Delta: 2 changed, 417 added, 0 deleted
[PERF] js_of_state: 5.00ms
[PERF] Payload size: 112210 bytes (109.58 KB)
[PERF] postMessage_send: 1.00ms
[PERF] send_to_parent: 9.00ms
[PERF] send_state_total: 15.00ms
[PERF] Received state from iframe at 406850.00ms (112188 bytes / 109.56 KB)
[PERF] Received 419 pieces as map (no conversion needed)
[PERF] doesStateEqualDoc check: 2.00ms (equal=false)
[PERF] UUID-based diff took: 1.00ms (changed: 2, added: 417)
[PERF] handle.change() MAP UPDATES took: 829.00ms (changed: 2, added: 417)
[PERF] Total state processing: 832.00ms
[PERF] Skipping echo - change originated from iframe
```

### Receiver

```
[PERF] Doc changed: 419 affected pieces, titleChanged=false
[PERF] Sending delta to iframe: 419 pieces, 112188 bytes (109.56 KB)
[PERF] Total send to iframe took: 3.40ms
[PERF] flatdoc_of_hazeldoc: 2.40ms
[PERF] Received delta with 419 pieces
[PERF] receive_state_total: 2.60ms
[PERF] zip_current: 0.10ms
[PERF] seg_to_doc traversed 1267 pieces
[PERF] seg_to_doc_current: 1.80ms
[PERF] merge_docs: 0.20ms
[PERF] Merged doc has 1685 pieces
[PERF] doc_to_seg reconstructed 1684 pieces
[PERF] doc_to_seg_merged: 1.00ms
[PERF] unzip_segment (1684 pieces): 0.00ms
[PERF] cursor_repositioning: 39.10ms
[PERF] sync_replace_total: 44.00ms
```

---

## Summary Tables

### Sender Side Breakdown

| Operation | Single Char (Warmup) | Single Char (Steady) | Large Paste (419 pcs) |
|-----------|---------------------|---------------------|----------------------|
| old_seg_to_doc | 4ms | 3ms | 4ms |
| seg_to_doc | 3ms | 2ms | 1ms |
| compute_delta | 3ms | 3ms | 3ms |
| js_of_state | 0ms | 0ms | 5ms |
| send_state_total | 12ms | 11ms | 15ms |
| **handle.change()** | **169ms** | **29ms** | **829ms** |
| Total processing | 171ms | 32ms | 832ms |
| Payload size | 302 B | 307 B | 110 KB |

### Receiver Side Breakdown

| Operation | Single Char (Warmup) | Single Char (Steady) | Large Paste (419 pcs) |
|-----------|---------------------|---------------------|----------------------|
| Total send to iframe | 3.1ms | 3.6ms | 3.4ms |
| flatdoc_of_hazeldoc | 0.1ms | 0.1ms | 2.4ms |
| seg_to_doc_current | 2.4ms | 1.8ms | 1.8ms |
| merge_docs | 0ms | 0ms | 0.2ms |
| doc_to_seg_merged | 0.6ms | 0.6ms | 1.0ms |
| **cursor_repositioning** | **33.6ms** | **32.3ms** | **39.1ms** |
| sync_replace_total | 38.5ms | 36.6ms | 44.0ms |

### Scaling Comparison

| Metric | Single Char | Large Paste | Scale Factor |
|--------|-------------|-------------|--------------|
| Pieces changed/added | 1 | 419 | 419x |
| Payload size | 0.3 KB | 110 KB | 367x |
| Hazel send_state_total | 11ms | 15ms | 1.4x |
| handle.change() | 29ms | 829ms | 29x |
| Receiver sync_replace | 37ms | 44ms | 1.2x |
| cursor_repositioning | 33ms | 39ms | 1.2x |

---

## Analysis

### What Scales Well

- **Hazel delta computation**: 11ms → 15ms for 419x more pieces (excellent)
- **Receiver sync_replace**: 37ms → 44ms (most of this is cursor_repositioning baseline)
- **Delta protocol**: Payload scales with changes, not document size

### Bottlenecks

1. **`handle.change()` (Automerge)**: ~2ms per piece added. For 419 pieces, this is 829ms. This is the dominant cost for any non-trivial edit.

2. **`cursor_repositioning`**: ~33ms baseline regardless of change size. Likely linear in cursor position within document, not in change size. Worth investigating but lower priority than handle.change().

3. **Double `seg_to_doc`**: Currently flatten both old and new state (~6ms combined). Could cache old flat doc to save ~3ms per edit.

### Observations

- Merged doc sometimes has 1 more piece than reconstructed (1265 vs 1264, 1685 vs 1684) - orphaned pieces filtered during doc_to_seg
- First action after reload has ~140ms extra warmup cost in handle.change()
- Payload reduction working well: single char change is ~300 bytes vs ~400KB+ for full state

---

## Future Optimization Targets

1. **handle.change() performance** - Investigate Automerge map update overhead, consider batching or different data structure
2. **cursor_repositioning** - Profile to understand the ~33ms baseline cost
3. **Cache old flat doc** - Eliminate redundant seg_to_doc call (~3ms savings)
