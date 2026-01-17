# Player Refractor

## Overview

The **Player refractor** allows playing Sound values from any expression in the program, not just the final result. Like Probe and Statics, it renders in the offside (sidebar) area rather than replacing syntax.

## Key Design Decisions

- **Refractor type**: Additive decoration (like Probe/Statics), not syntax-replacing
- **Target**: Expressions with type `Sound` (checked via statics)
- **Mutual exclusion**: Global play state - multiple Players can exist, but only one plays at a time
- **Keyboard shortcut**: Alt+P (Option+P on Mac)

## Files

### Created
- `src/haz3lcore/SoundUtil.re` - Sound interpretation utilities (extracted from EvalResult)
- `src/haz3lcore/projectors/implementations/PlayerProj.re` - Main projector implementation
- `src/web/www/style/projectors/proj-player.css` - Styling

### Modified
- `src/haz3lcore/projectors/ProjectorCore.re` - Added `Player` to Kind.t and refractors list
- `src/haz3lcore/projectors/ProjectorInit.re` - Added module mapping
- `src/haz3lcore/zipper/action/Action.re` - Added `TogglePlayer` to probe actions
- `src/haz3lcore/ProbePerform.re` - Added `can_player`, `toggle_player`, probe_status handling
- `src/web/app/editors/code/ContextMenu.re` - Added context menu integration
- `src/web/Keyboard.re` - Added Alt+P keyboard shortcut
- `src/web/app/input/Shortcut.re` - Registered shortcut in command palette
- `src/web/www/style/projectors/proj-base.css` - Added CSS import
- `src/web/app/editors/result/EvalResult.re` - Removed old audio_view (replaced by Player)

## Implementation Notes

### Type Checking
Player only appears in context menu for expressions with type `Sound`. The check is in `ProbePerform.can_player`:
```reason
let rec is_sound_type = (ty: Typ.t): bool =>
  switch (ty.term) {
  | Var("Sound") => true
  | Parens(inner) => is_sound_type(inner)
  | _ => false
  };
```

### Global Play State
Only one Player can play at a time. Starting a new Player stops any currently playing audio:
```reason
module PlayState = {
  let current: ref(option(Id.t)) = ref(Option.None);
  let play = (id, pattern) => { Strudel.stopMusic(); Strudel.playPattern(pattern); current := Option.Some(id); };
  let stop = () => { Strudel.stopMusic(); current := Option.None; };
  let is_playing = (id) => current^ == Option.Some(id);
};
```

## Testing Checklist

- [x] Player appears in context menu only for Sound-typed expressions
- [x] Play button starts audio
- [x] Pause button stops audio
- [x] Starting one Player stops another
- [x] Removing Player while playing stops audio
- [x] Works with Sound constructors (Note, Sample, Rev, Fast, Slow, Seq, Stack, JuxRev)
- [x] Shows empty indicator when no samples available
- [x] Keyboard shortcut Alt+P works
