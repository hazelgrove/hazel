# Zen Mode Plan

## Status Summary

### ✅ Completed
- **Core implementation** - CSS-based overlay showing only main editor panel
- **Toggle shortcut** - Cmd-K (Mac) / Ctrl-K (PC) toggles zen mode
- **Patchwork default** - Zen mode on by default in iframe, off otherwise

### ⏳ To Do
- **Exit discoverability** - Users may get stuck without knowing how to exit
- **Command palette conflict** - Cmd-K conflicts with ninja-keys palette shortcut
- **Add to command palette** - "Toggle Zen Mode" entry in ninja-keys

---

## Current Implementation

### Files Touched (3 files, ~22 lines)
- **Settings.re:8,23,111,331-334** - `zen: bool` setting, defaults to `is_in_iframe()`
- **Page.re:430-432,766** - Cmd-K/Ctrl-K handler, adds "zen" class to #main
- **style.css:102-106** - `#main.zen` spans full grid with z-index 40

### How It Works
The implementation is CSS-only: when `settings.zen` is true, the "zen" class is added to `#main`, which then spans the entire grid (rows 1-3, columns 1-2) with z-index 40, overlaying sidebar/top-bar/bottom-bar. Hidden elements are still rendered, just covered.

### Relevant Commit
```
668034aee simple zen mode. shows only main panel. enabled by default. cmd-k to toggle
```

---

## Problem: Exit Discoverability

Users who accidentally enter zen mode (or enter it without knowing the shortcut) may not know how to exit. The UI elements that might indicate how to exit are hidden by zen mode itself.

### VS Code's Solution
Microsoft specifically addressed this because users kept getting stuck:

1. **Double-Escape** - Primary exit method, very discoverable
2. **Same chord shortcut** - Cmd-K Z (chord: K then Z sequentially)
3. **Command palette** - Cmd-Shift-P → "Toggle Zen Mode"

---

## Options for Exit Discoverability

### Option A: Double-Escape Exit (Recommended)
- Escape is the universal "get me out of here" key
- Two presses prevents accidental exit
- Follows VS Code precedent
- Very discoverable even without knowing entry shortcut

### Option B: Change to Chord Shortcut
- Use Cmd-K Z like VS Code
- Fixes command palette conflict (Cmd-K would open palette)
- Implies familiarity with command palette system

### Option C: Transient Toast Notification
- Show "Press Escape twice to exit Zen mode" on entry
- Fades after 3-5 seconds
- More implementation overhead

### Option D: Command Palette Only
- Remove direct keyboard shortcut
- Entry/exit only via command palette
- Ensures users know how to use palette
- Reduces discoverability for entering

---

## Problem: Command Palette Shortcut Conflict

The tooltip at `Page.re:636` indicates command palette should open with Cmd-K:
```reason
~tooltip="Command Palette (" ++ Keyboard.meta(Os.is_mac^ ? Mac : PC) ++ " + k)"
```

But `Page.re:430-432` intercepts Cmd-K for zen mode toggle before ninja-keys can handle it. Currently the command palette can only be opened via button click.

### Options
1. **Change zen mode to chord** - Cmd-K Z (free up Cmd-K for palette)
2. **Change palette shortcut** - Cmd-Shift-K or Cmd-P
3. **Keep conflict** - Palette accessible via button only

---

## Proposed Implementation (Minimal Mergeable)

For initial merge, implement double-Escape exit:

### Phase 1: Double-Escape Exit

Add tracking for last escape press time in Page.re:

```reason
(* In Selection.handle_key_event *)
| {key: D("Escape"), ...} when globals.settings.zen =>
  (* Check if second escape within 500ms *)
  if (within_double_tap_window()) {
    Some(Update.Globals(Set(Zen)))
  } else {
    record_escape_time();
    None
  }
```

### Phase 2: Add to Command Palette

Add entry to `Shortcut.re`:
```reason
mk_shortcut(
  ~section="Settings",
  ~mdIcon="fullscreen_exit",
  ~hotkey=Keyboard.meta(sys) ++ "+k",  (* or different if resolving conflict *)
  "Toggle Zen Mode",
  Globals(Set(Zen)),
),
```

### Phase 3 (Optional): Resolve Shortcut Conflict

Either:
- Change zen mode to Cmd-K Z chord
- Or change command palette to Cmd-Shift-K

---

## Performance Note

Current CSS approach keeps hidden elements rendered. A more optimal implementation would conditionally render components, but CSS overlay is acceptable for initial merge.

---

## Breaking Out to Separate Branch

To isolate zen mode changes for a separate PR:

```bash
git checkout dev
git checkout -b zen-mode
git cherry-pick 668034aee  # original zen mode commit
git cherry-pick c0c8587ba  # (partial) - only Settings.re zen default change
# Then add exit discoverability improvements
```

Note: `c0c8587ba` has other changes beyond zen mode (iframe guards, doc slides skip, etc.) so may need selective cherry-pick or manual extraction.
