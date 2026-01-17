# Strudel Integration Plan for Hazel

## Overview

[Strudel](https://strudel.cc/) is a browser-based live coding environment that ports TidalCycles' pattern language to JavaScript. It enables algorithmic music composition through pattern manipulation.

**Goal:** Bring Strudel's pattern-based music creation into Hazel with:
- Native Hazel types for audio patterns
- Inline projectors for visual/interactive pattern editing
- Seamless integration with Hazel's existing type system and evaluation

## Current State

### What's Implemented
- `Sound` type with constructors: `Note(String)`, `Rev(Sound)`, `Fast(Float, Sound)`, `Slow(Float, Sound)`, `Seq(List(Sound))`, `Stack(List(Sound))`
- Strudel JS library loaded via unpkg in `index.html`
- `Strudel.initOnLoad()` called on startup in `Main.re`
- `audio_view` wired into `live_eval` - shows play/stop controls (▶/■) for Sound values
- Defensive JS bindings in `src/util/Strudel.re` with proper initialization
- CSS styling in `style/strudel.css` matching UI aesthetic
- **Note Picker projector** - piano keyboard for selecting notes (constructor-level: `^^notes(Note(...))`)
- **Rhythm Grid projector** - step sequencer for drum patterns (constructor-level: `^^rhythm(Note(...))`)
- Context menu shows all applicable projectors (not just first)
- Block placeholders for projectors (piano: 4 rows, rhythm: 6 rows)

### What's In Progress
- Execution visualization (highlighting notes as they play) - deferred due to `onTrigger` callback issues
- More projector types (knobs, chord pickers, etc.)

### What's Missing (Future)
- Scale picker projector
- Parameter sliders (gain, pan, filter)
- Euclidean rhythm projector
- Full piano roll timeline editor
- Wiring between projectors

---

## Phase 1: MVP - Get Sound Playing (CURRENT)

### 1.1 Wire up audio_view
Connect `audio_view` in `EvalResult.re` so that when a program evaluates to `Note("pattern")`, it:
- Shows play/stop buttons
- Auto-plays the pattern (with user interaction gate for browser audio policy)

**File:** `src/web/app/editors/result/EvalResult.re`

### 1.2 Basic Console/REPL Testing
Verify from browser console:
```javascript
window.initStrudel()
window.note("c4 e4 g4").play()
window.hush()
```

### 1.3 Test in Hazel
Write and evaluate:
```
Note("c4 e4 g4 c5")
```
Should show play/stop UI and produce sound.

---

## Phase 2: Expand Sound Type & API

### 2.1 Richer Sound ADT

```
type Sound =
  | Note(String)           // Mini-notation pattern
  | Chord(String)          // Chord symbol like "Cmaj7"
  | Scale(String, String)  // (root, scale_type) like ("C", "minor")
  | Seq(List(Sound))       // Sequential composition
  | Stack(List(Sound))     // Parallel composition (play together)
  | Silence               // Rest
```

**ASCII Diagram - Sound Composition:**
```
┌─────────────────────────────────────────────────────┐
│  Stack([                                            │
│    Note("c4 e4 g4"),      ─┐                       │
│    Note("~ ~ c3 ~")        ├─► Plays simultaneously │
│  ])                       ─┘                       │
└─────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────┐
│  Seq([                                              │
│    Note("c4 d4"),  ──►  Note("e4 f4")  ──►  ...    │
│  ])                                                 │
│  Plays one after another                            │
└─────────────────────────────────────────────────────┘
```

### 2.2 Hazel Functions Wrapping Strudel

Add builtin functions that map to Strudel operations:

```
// Pattern creation
note : String -> Sound
sound : String -> Sound
n : Int -> Sound

// Transformations
rev : Sound -> Sound           // Reverse pattern
fast : (Float, Sound) -> Sound // Speed up
slow : (Float, Sound) -> Sound // Slow down
jux : (Sound -> Sound, Sound) -> Sound  // Juxtapose with transformation

// Scales
scale : (String, Sound) -> Sound  // Apply scale to pattern
transpose : (Int, Sound) -> Sound // Transpose by semitones

// Effects
gain : (Float, Sound) -> Sound
pan : (Float, Sound) -> Sound
lpf : (Float, Sound) -> Sound    // Low-pass filter
```

---

## Phase 3: Projectors for Pattern Editing

### 3.1 Note Picker Projector

A piano-roll style widget for selecting notes visually.

**ASCII Diagram:**
```
┌─────────────────────────────────────────────┐
│  Note Picker                          [×]   │
├─────────────────────────────────────────────┤
│  ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐    │
│  │ │█│ │█│ │ │█│ │█│ │█│ │ │█│ │█│ │ │    │
│  │ │ │ │ │ │ │ │ │ │ │ │ │ │ │ │ │ │ │    │
│  ├─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤    │
│  │C│ │D│ │E│F│ │G│ │A│ │B│C│ │D│ │E│F│    │
│  └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘    │
│                                             │
│  Selected: C4  E4  G4                       │
│  Output: "c4 e4 g4"                         │
└─────────────────────────────────────────────┘
```

**Triggers on:** `String` arguments to `note()` or `Note()` constructor

### 3.2 Rhythm Grid Projector

A step sequencer style grid for rhythmic patterns.

**ASCII Diagram:**
```
┌──────────────────────────────────────────────┐
│  Rhythm Grid (8 steps)               [×]     │
├──────────────────────────────────────────────┤
│  BD  [●][○][○][○][●][○][○][○]               │
│  SD  [○][○][●][○][○][○][●][○]               │
│  HH  [●][●][●][●][●][●][●][●]               │
├──────────────────────────────────────────────┤
│  Output: "bd ~ sd ~ bd ~ sd ~"               │
└──────────────────────────────────────────────┘
```

**Triggers on:** `String` arguments with drum patterns (bd, sd, hh, etc.)

### 3.3 Slider Projectors for Parameters

Reuse existing slider projectors for:
- `gain`: 0.0 - 1.0
- `pan`: -1.0 - 1.0
- `lpf`/`hpf`: 20 - 20000 Hz (log scale)
- `fast`/`slow`: 0.25 - 4.0

**ASCII Diagram:**
```
┌─────────────────────────────────────────────┐
│  gain(|████████░░| 0.8, Note("c4"))         │
│        ▲                                    │
│        └── Inline slider projector          │
└─────────────────────────────────────────────┘
```

### 3.4 Scale/Chord Picker Projector

Dropdown or wheel picker for scales and chords.

**ASCII Diagram:**
```
┌─────────────────────────────────────────┐
│  scale("C", |minor ▼|, n("0 2 4 7"))    │
│              ┌──────────┐               │
│              │ major    │               │
│              │►minor    │               │
│              │ dorian   │               │
│              │ phrygian │               │
│              │ lydian   │               │
│              └──────────┘               │
└─────────────────────────────────────────┘
```

### 3.5 Euclidean Rhythm Projector

Visual editor for euclidean rhythms `(beats, steps, offset)`.

**ASCII Diagram:**
```
┌─────────────────────────────────────────────┐
│  Euclidean: (5, 8, 0)                       │
│                                             │
│         ●                                   │
│       ●   ○                                 │
│      ○     ●        Beats: [5] ◄──►        │
│       ●   ○         Steps: [8] ◄──►        │
│         ●           Offset:[0] ◄──►        │
│                                             │
│  Pattern: "x ~ x x ~ x x ~"                 │
└─────────────────────────────────────────────┘
```

---

## Phase 4: Live Performance Features

### 4.1 Transport Controls
- Global play/pause/stop
- BPM/CPS control
- Tap tempo

### 4.2 Pattern Visualization
- Waveform display
- Piano roll visualization of playing notes
- Pattern cycle indicator

### 4.3 MIDI Output (Future)
- Send patterns to external synths
- MIDI learn for projector controls

---

## Phase 5: Modular Synth Vision

### 5.1 Design Philosophy
Goal: Create a **modular synthesizer aesthetic** where multiple projectors can be arranged visually like rack-mounted modules, with eventual support for wiring between them.

**Key concepts:**
- Tab-style projectors allow multiple block-shaped UIs to stack horizontally on a row
- Various sizes for different controls (inline knobs vs block-sized keyboards)
- Visual cohesion across the "rack" of controls

### 5.2 Expanded Projector Types

**Small inline (1-2 rows):**
- **Knobs/Dials**: Circular sliders for gain, pan, filter cutoff
- **XY Pad**: 2D control surface (e.g., filter cutoff + resonance)
- **Waveform selector**: Icons for sine/saw/square/triangle
- **Toggle switches**: Effect on/off

**Medium block (3-6 rows):**
- **Step sequencer**: Timeline grid (notes × time) - allows repetition unlike piano toggle
- **Chord picker**: Circle of fifths, chord buttons (Cmaj, Dm7, etc.)
- **Scale selector**: Wheel or dropdown (major, minor, dorian, etc.)
- **Envelope (ADSR)**: Draggable attack/decay/sustain/release curve
- **Sample browser**: Grid of drum/synth samples

**Large tab (takes full row):**
- **Piano roll**: Full timeline editor with draggable notes
- **Mixer**: Multi-track with faders, pan, effects per track
- **Spectrogram**: Real-time frequency visualization
- **Waveform display**: Oscilloscope showing audio output

### 5.3 Wiring Between Projectors (Exploratory)

**Concept:** Visually connect projector outputs to inputs via "wires" that represent variable bindings in the underlying syntax.

**How it maps to Hazel:**
- Projector on a **pattern** (let binding): `let x = ^^knob(0.5) in ...` - the wire "source"
- Projector on an **expression** (variable reference): `gain(x, ...)` - the wire "destination"
- The wire is a visual representation of the variable `x`

**Interaction model:**
- Unplugging a wire → replaces the variable reference with a hole
- Plugging in a wire → replaces hole with the variable
- Switching wires → syntax transformation to use different variable

**Why this is principled:**
- Wires aren't arbitrary connections; they're visual syntax for variable bindings
- No new semantics - just a different view of existing let/var structure
- Similar to Max/MSP or Pure Data, but with functional semantics instead of signal processing

### 5.4 Audio Patching vs Dataflow

**Dataflow (what Hazel/Strudel does):**
- Values flow through expressions: `Fast((2.0, Note("c4")))` → evaluate inner, transform result
- Pure functional: same input always gives same output
- Explicit composition via syntax: `Stack([a, b])` means "play a and b together"
- Time is implicit in pattern structure
- Variables are **bindings**: immutable references

**Audio patching (traditional modular synths):**
- Signals flow through modules: oscillator → filter → amplifier → speaker
- Stateful: modules have internal state (envelopes, LFOs, sequencer position)
- Implicit routing via cables: physically connect output jack to input jack
- Time is explicit: audio rate (44.1kHz) vs control rate (variable)
- Cables are **signal streams**: continuous voltages, not discrete values

**Key insight:** Dataflow describes **what to play**, patching describes **how sound is generated**. Strudel/Hazel operates at the pattern/composition level (dataflow), while traditional modular is at signal/synthesis level.

---

## Implementation Priority

### MVP (Do First) - COMPLETE
1. ✅ Sound type defined
2. ✅ Strudel JS loaded
3. ✅ Wire `audio_view` into `live_eval`
4. ✅ Test `Note("c4 e4 g4")` plays sound

### Short Term - COMPLETE
5. ✅ Add `rev`, `fast`, `slow` functions (as Sound ADT constructors)
6. ✅ Add `Seq` and `Stack` to Sound type
7. ✅ Basic note picker projector (single octave)

### Medium Term (CURRENT)
8. ✅ Rhythm grid projector
9. ✅ UI polish: Block placeholders, contained black keys, symbol buttons
10. ✅ Context menu shows all applicable projectors (not just first)
11. ⬜ Scale picker projector
12. ⬜ Parameter sliders (gain, pan, filter)

### Long Term
11. ⬜ Euclidean rhythm projector
12. ⬜ Full piano keyboard projector
13. ⬜ Pattern visualization
14. ⬜ Transport controls

---

## File Locations

| Component | File |
|-----------|------|
| Sound type | `src/language/builtins/BuiltinsADT.re` |
| Strudel JS bindings | `src/util/Strudel.re` |
| Audio view rendering | `src/web/app/editors/result/EvalResult.re` |
| Projector base | `src/haz3lcore/projectors/ProjectorBase.re` |
| Projector implementations | `src/haz3lcore/projectors/implementations/` |
| Projector views | `src/web/app/common/ProjectorView.re` |

---

## References

- [Strudel Documentation](https://strudel.cc/)
- [Mini Notation Guide](https://strudel.cc/learn/mini-notation/)
- [Tonal Functions](https://strudel.cc/learn/tonal/)
- [Strudel Source (Codeberg)](https://codeberg.org/uzu/strudel)
- [TidalCycles](https://tidalcycles.org/)

---

## Questions to Resolve

1. **Auto-play vs Click-to-play:** Browser audio policies require user interaction. Should we auto-play on first user click, or always require explicit play button?

2. **Pattern Composition Model:** Should `Seq` and `Stack` be constructors, or should we have a more functional approach with `seq : List(Sound) -> Sound`?

3. **Projector Activation:** How should projectors know to activate? Options:
   - Type-based (any String in Note context)
   - Explicit projector syntax
   - Contextual menu option

4. **Real-time Updates:** Should pattern changes while playing cause immediate audio updates, or require re-triggering?
