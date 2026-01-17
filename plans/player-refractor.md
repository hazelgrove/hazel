# Player Refractor Implementation Plan

## Overview

Add a **Player refractor** that allows playing Sound values from any expression in the program, not just the final result. Like Probe and Statics, it renders in the offside (sidebar) area rather than replacing syntax.

## Concept

- **Refractor type**: Additive decoration (like Probe/Statics), not syntax-replacing
- **Target**: Expressions that evaluate to the `Sound` type
- **UI**: Play/pause controls in the offside area, similar to probe value display
- **Dynamics**: Uses evaluated value from dynamics samples
- **Mutual exclusion**: Only one Player can be playing at a time

## Design Decisions

### Mutual Exclusion Strategy

Two approaches considered:

**Option A: Global play state (recommended)**
- Multiple Players can exist, but only one plays at a time
- Global ref tracks which Player (if any) is currently playing
- Starting one Player stops any currently playing audio
- Simpler implementation, no zipper modification needed

**Option B: Only one Player allowed**
- Adding a new Player removes any existing Players
- Would require iterating through refractors in zipper when adding
- More complex, but guarantees single Player

**Chosen: Option A** - Global state is simpler and more flexible. Users can keep multiple Players to quickly switch between different parts of their composition.

```reason
module PlayState = {
  let current: ref(option(Id.t)) = ref(None);

  let play = (id: Id.t, pattern: Strudel.pattern) => {
    Strudel.stopMusic();
    Strudel.playPattern(pattern);
    current := Some(id);
  };

  let stop = () => {
    Strudel.stopMusic();
    current := None;
  };

  let is_playing = (id: Id.t) => current^ == Some(id);
};
```

### Type Restriction

Only show in context menu for expressions with `Sound` type. Check via statics:

```reason
let is_sound_type = (statics: option(Info.t)): bool =>
  switch (statics) {
  | Some(InfoExp({self, _})) =>
    switch (Self.typ_of_exp(self)) {
    | Some({term: Sum(_), _} as typ) =>
      /* Check if it's the Sound sum type */
      Typ.eq(typ, BuiltinsADT.sound_type)
    | _ => false
    }
  | _ => false
  };
```

### Accessing the Sound Value

Use dynamics samples like Probe does. The Player needs `dynamics = true` in the module.

```reason
let get_sound_value = (dynamics: option(Dynamics.Info.t)): option(Exp.t) =>
  switch (dynamics) {
  | Some(di) =>
    switch (di.samples) {
    | [sample, ..._] => Some(sample.value)  /* Take first sample */
    | [] => None
    }
  | None => None
  };
```

Then reuse `interpret_sound` and `is_sound` from EvalResult.re (will need to extract to a shared module).

## Files to Create/Modify

### New Files

1. **`src/haz3lcore/projectors/implementations/PlayerProj.re`**
   - The main projector implementation
   - Similar structure to TypeProj.re but with dynamics

2. **`src/web/www/style/projectors/proj-player.css`**
   - Styling for play/pause controls in offside area
   - Consistent with Hazel earth tones + green accents

### Modified Files

1. **`src/haz3lcore/projectors/ProjectorCore.re`**
   - Add `Player` to `Kind.t` enum
   - Add to `refractors` list (not `livelit_projectors`)
   - Add `name` and `of_name` cases

2. **`src/haz3lcore/projectors/ProjectorInit.re`**
   - Add `Player => (module Cook(PlayerProj.M))` case

3. **`src/web/app/editors/code/ContextMenu.re`**
   - Add display name for Player

4. **`src/web/www/style/projectors/proj-base.css`**
   - Add `@import "proj-player.css";`

5. **`src/util/SoundUtil.re`** (new shared module)
   - Extract `is_sound`, `interpret_sound`, `sound_description` from EvalResult.re
   - Make available to both EvalResult.re and PlayerProj.re

6. **`src/web/app/editors/result/EvalResult.re`**
   - Import from SoundUtil.re instead of defining locally

## Implementation Details

### PlayerProj.re Structure

```reason
open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* Global play state for mutual exclusion */
module PlayState = {
  let current: ref(option(Id.t)) = ref(None);

  let play = (id: Id.t, pattern: Strudel.pattern) => {
    Strudel.stopMusic();
    Strudel.playPattern(pattern);
    current := Some(id);
  };

  let stop = () => {
    Strudel.stopMusic();
    current := None;
  };

  let is_playing = (id: Id.t) => current^ == Some(id);
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Play
    | Stop;

  /* Only init on Sound-typed expressions */
  let init = (any: Language.Any.t): option(model) =>
    switch (any) {
    | Exp(_) => Some()  /* Will refine with type check */
    | _ => None
    };

  let dynamics = true;  /* Need dynamics to get evaluated value */
  let focusable = Focusable.non;
  let placeholder = (_, _) => ProjectorCore.Shape.default;

  let update = (model, _info, action) =>
    switch (action) {
    | Play | Stop => model  /* State is global, not in model */
    };

  let view = ({info, local, _}: View.args(model, action)) => {
    let sound_value = get_sound_value(info.dynamics);
    let is_playing = PlayState.is_playing(info.id);

    View.{
      inline: Node.div([]),
      overlay: None,
      offside: Some(
        Node.div(
          ~attrs=[Attr.classes(["player-offside"])],
          [
            /* Play/Pause button */
            Node.button(
              ~attrs=[
                Attr.classes(["player-btn", is_playing ? "playing" : ""]),
                Attr.on_click(_ =>
                  if (is_playing) {
                    PlayState.stop();
                    local(Stop);
                  } else {
                    switch (sound_value) {
                    | Some(exp) =>
                      switch (SoundUtil.interpret_sound(exp)) {
                      | Some(pattern) =>
                        PlayState.play(info.id, pattern);
                        local(Play);
                      | None => Effect.Ignore
                      }
                    | None => Effect.Ignore
                    }
                  }
                ),
              ],
              [Node.text(is_playing ? "⏸" : "▶")],
            ),
            /* Optional: Show sound description */
            switch (sound_value) {
            | Some(exp) =>
              Node.span(
                ~attrs=[Attr.classes(["player-desc"])],
                [Node.text(SoundUtil.sound_description(exp))]
              )
            | None =>
              Node.span(
                ~attrs=[Attr.classes(["player-desc", "no-sound"])],
                [Node.text("∅")]
              )
            },
          ],
        )
      ),
    };
  };
};
```

### CSS Styling (proj-player.css)

```css
/* Player refractor - audio controls in offside */

.player-offside {
  display: flex;
  align-items: center;
  gap: 0.4em;
  padding: 0.2em 0.4em;
  background: var(--T2);
  border-radius: 0.3em;
  border: 1px solid var(--BR2);
}

.player-btn {
  width: 1.6em;
  height: 1.6em;
  border: 1px solid var(--BR2);
  border-radius: 0.2em;
  background: var(--T1);
  color: var(--STONE);
  cursor: pointer;
  font-size: 0.9em;
  display: flex;
  align-items: center;
  justify-content: center;
  transition: background-color 0.1s, border-color 0.1s;
}

.player-btn:hover {
  background: var(--T3);
  border-color: var(--BR3);
}

.player-btn.playing {
  background: var(--G0);
  border-color: var(--G0);
  color: white;
}

.player-btn.playing:hover {
  background: var(--G1);
}

.player-desc {
  font-family: var(--code-font);
  font-size: 0.75em;
  color: var(--STONE);
  max-width: 15em;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.player-desc.no-sound {
  color: var(--BR3);
  font-style: italic;
}
```

## Implementation Order

1. **Extract SoundUtil.re** - Move `is_sound`, `interpret_sound`, `sound_description` from EvalResult.re
2. **Update EvalResult.re** - Import from SoundUtil.re
3. **Create PlayerProj.re** - Main implementation
4. **Register in ProjectorCore.re** - Add to Kind.t and refractors list
5. **Wire in ProjectorInit.re** - Add module mapping
6. **Update ContextMenu.re** - Add display name
7. **Create proj-player.css** - Styling
8. **Update proj-base.css** - Add import
9. **Test** - Verify mutual exclusion, play/stop, cleanup on removal

## Edge Cases

1. **Refractor removed while playing**: Need to stop sound if this Player was playing. This can be handled in the action handler for `Remove` - check if `PlayState.is_playing(id)` and call `PlayState.stop()`. Alternatively, accept that sound continues until manually stopped or another Player starts (simpler but less clean).

2. **Program changes while playing**: The dynamics will update with new samples. The displayed description updates, but the playing sound continues until stop/re-play.

3. **No samples yet (expression not evaluated)**: Show "∅" indicator similar to Probe's empty state.

4. **Non-Sound expression**: The `init` function should check type via statics and return `None` for non-Sound expressions, preventing the projector from being added.

## Migration: Remove Results Panel Audio

Once the Player refractor is working, remove the existing `audio_view` from the results panel:

1. **Delete from EvalResult.re**:
   - Remove `audio_view` function
   - Remove `last_sound` ref
   - Remove audio controls rendering in `live_eval`
   - Keep `SoundUtil.re` for the Player refractor

2. **Delete strudel.css** (or repurpose for Player):
   - The results panel styling is no longer needed

3. **Rationale**:
   - Player refractor is more flexible (play any Sound, not just final result)
   - Cleaner results panel (just shows the evaluated value)
   - Consistent with Hazel philosophy (probing/inspecting intermediate values)

## Future Enhancements

- **Waveform visualization**: Show a small waveform or pattern visualization inline
- **Volume control**: Add a mini knob for volume
- **Progress indicator**: Show cycle position within the pattern
- **Multiple tracks**: Allow simultaneous playback (would need to remove mutual exclusion)

## Testing Checklist

- [ ] Player appears in context menu only for Sound-typed expressions
- [ ] Play button starts audio
- [ ] Pause button stops audio
- [ ] Starting one Player stops another
- [ ] Removing Player while playing stops audio
- [ ] Works with all Sound constructors (Note, Sample, Rev, Fast, Slow, Seq, Stack)
- [ ] Shows "∅" when no samples available
- [ ] Styling matches other refractors (Probe, Statics)
