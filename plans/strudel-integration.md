# Strudel Integration Plan for Hazel

## Overview

[Strudel](https://strudel.cc/) is a browser-based live coding environment that ports TidalCycles' pattern language to JavaScript. It enables algorithmic music composition through pattern manipulation.

**Goal:** Bring Strudel's pattern-based music creation into Hazel with:
- Native Hazel types for audio patterns
- Inline projectors for visual/interactive pattern editing
- Seamless integration with Hazel's existing type system and evaluation

---

## Strudel Language Background

### Core Design

Strudel operates on a functional reactive programming model where **patterns are pure functions** from time spans to events. From the [technical documentation](https://strudel.cc/technical-manual/patterns/):

> "The query function itself constitutes the pattern—there is no way to change a pattern, it is opaque as a pure function."

Patterns are queried via `pattern.queryArc(startCycle, endCycle)` returning events with:
- **Value**: the data (note, sample name, parameters)
- **Begin/End times**: as fractions of cycles

### Method Chaining Style

Strudel programs use **fluent method chaining**:

```javascript
s("bd sd hh sd")
  .bank("RolandTR909")
  .fast(2)
  .gain(0.8)
  .room(0.15)
  .jux(rev)
```

Each method returns a new Pattern that can be chained. The pattern flows through transformations with the pattern as implicit receiver (`this`).

### Key Functions

| Strudel | Purpose |
|---------|---------|
| `note("c4 e4")` | Melodic pattern (synth) |
| `s("bd sd")` / `sound("bd sd")` | Sample pattern (drums, etc.) |
| `.fast(n)` / `.slow(n)` | Speed modification |
| `.gain(n)` | Volume (0-1) |
| `.pan(n)` | Stereo position (-1 to 1) |
| `.bank("name")` | Sample bank selection |
| `.jux(fn)` | Stereo split with transformation |
| `.rev()` | Reverse pattern |
| `stack(a, b, ...)` | Play simultaneously |
| `seq(a, b, ...)` / `cat(a, b, ...)` | Play sequentially |

### Mini-Notation

Strudel's mini-notation (inherited from TidalCycles) allows compact rhythm expression:

| Syntax | Meaning | Example |
|--------|---------|---------|
| `*n` | Repeat n times | `"bd*4"` = four bass drums |
| `/n` | Slow by n | `"hh/2"` = half-speed |
| `[ ]` | Group | `"bd [sd hh]"` = bd, then sd+hh together |
| `< >` | Alternate | `"<bd sd>"` = bd first cycle, sd next |
| `~` | Rest | `"bd ~ sd ~"` |
| `:n` | Sample variation | `"hh:0 hh:1"` |
| `,` | Parallel in group | `"[bd, hh*4]"` |

### Sample Banks

Banks group related samples under a common prefix:
```javascript
s("bd sd").bank("RolandTR808")  // equivalent to s("RolandTR808_bd RolandTR808_sd")
```

Common banks: `RolandTR808`, `RolandTR909`, `RolandTR707`

### Live Coding Behavior

Strudel's scheduler runs continuously at ~50ms intervals. When code is edited:
1. New code is transpiled and evaluated to create new Pattern
2. The scheduler's next tick queries the new pattern
3. **Clock is preserved**—no restart, seamless transition
4. Latency is approximately 50-150ms

This enables live performance where edits don't cause audio gaps

## Current State

### What's Implemented
- `Sound` type with constructors: `Note(String)`, `Sample(String)`, `Rev(Sound)`, `Fast(Float, Sound)`, `Slow(Float, Sound)`, `Seq(List(Sound))`, `Stack(List(Sound))`, `JuxRev(Sound)`, `Gain(Float, Sound)`, `Pan(Float, Sound)`, `Bank(String, Sound)`, `Cpm(Float, Sound)`
- `Sample(String)` uses Strudel's `sound()` function for drums/samples (bd, sd, piano, etc.)
- `JuxRev(Sound)` applies stereo widening effect (jux with rev)
- Strudel JS library loaded via unpkg in `index.html`
- `Strudel.initOnLoad()` called on startup in `Main.re`
- **Dirt-Samples auto-loaded** via `prebake` callback in `initStrudel()` - loads from `github:tidalcycles/dirt-samples` with graceful degradation on network failure
- **Player refractor** - play/stop controls in offside for any Sound-typed expression
- **Live coding behavior** - pattern updates via `scheduler.setPattern` without audio gaps
- **Playback cleanup** - removing Player stops audio; PlayState managed in Strudel.re
- Defensive JS bindings in `src/util/Strudel.re` with proper initialization
- CSS styling in `style/strudel.css` matching UI aesthetic
- **Note Picker projector** - piano keyboard for selecting notes, normalizes flats to sharps
- **Rhythm Grid projector** - step sequencer for drum patterns
- **Knob projector** - rotary dial for Float values with modular synth aesthetic
- **XY Pad projector** - 2D control surface for (Float, Float) tuples
- Context menu shows all applicable projectors (not just first)
- Block placeholders for projectors (piano: 4 rows, rhythm: 6 rows, knob: 3 rows, xypad: 5 rows)

### What's In Progress
- Execution visualization (highlighting notes as they play) - deferred due to `onTrigger` callback issues

### What's Missing (Future)
- Scale picker projector
- Euclidean rhythm projector
- Full piano roll timeline editor
- Wiring between projectors

---

## Hazel vs Strudel: Design Divergences

### Syntax: Constructors vs Method Chains

**Our current approach (prefix constructors):**
```
Fast((2.0, Stack([Sample("bd sd"), Note("c4 e4")])))
```

**Strudel equivalent (postfix chaining):**
```javascript
stack(s("bd sd"), note("c4 e4")).fast(2)
```

| Aspect | Our Constructors | Strudel Chaining |
|--------|------------------|------------------|
| Reads as | "Fast of 2 applied to..." | "This pattern, made fast by 2" |
| Edit locality | Inner modifications ripple | Append transformations at end |
| Partial application | N/A for constructors | Built into method design |

Both are valid—constructors are more explicit, chaining is more ergonomic for live coding.

### Proposed: Pipeline-Style Functions

To enable Strudel-like chaining with Hazel's `|>` operator, we can add **curried wrapper functions**:

```
// Copy-pasteable Hazel program with curried wrappers:

let note: String -> Sound = fun s -> Note(s) in
let sample: String -> Sound = fun s -> Sample(s) in
let rev: Sound -> Sound = fun s -> Rev(s) in
let fast: Float -> Sound -> Sound = fun f -> fun s -> Fast((f, s)) in
let slow: Float -> Sound -> Sound = fun f -> fun s -> Slow((f, s)) in
let seq: List(Sound) -> Sound = fun sounds -> Seq(sounds) in
let stack: List(Sound) -> Sound = fun sounds -> Stack(sounds) in

// Example usage with pipeline:
let melody = note("c4 e4 g4 c5") in
let drums = sample("bd ~ sd ~") in

// Pipeline style (reads left-to-right like Strudel):
let fast_melody = melody |> fast(2.0) |> rev in

// Composition:
stack([fast_melody, drums])
```

**Key design choice:** Pattern is the **last argument** to enable `|> fast(2.0)` without underscores.

**Comparison:**
```
Strudel:                            Hazel Pipeline:
──────────────────────────────────────────────────────────────────
note("c4 e4")                       note("c4 e4")
  .fast(2)                            |> fast(2.0)
  .gain(0.8)                          |> gain(0.8)
  .jux(rev)                           |> juxRev

stack(                              stack([
  s("bd sd"),                         sample("bd sd"),
  note("c4")                          note("c4")
).fast(2)                           ]) |> fast(2.0)
```

This is **strictly additive**—constructors still work for those who prefer that style.

### Semantic Mapping

| Hazel | Strudel | Notes |
|-------|---------|-------|
| `Note(String)` | `note(str)` | Melodic synth tones |
| `Sample(String)` | `s(str)` / `sound(str)` | Drums, samples |
| `Rev(Sound)` | `.rev()` | Reverse pattern |
| `Fast((Float, Sound))` | `.fast(n)` | Speed up |
| `Slow((Float, Sound))` | `.slow(n)` | Slow down |
| `Seq(List(Sound))` | `seq(...)` / `cat(...)` | Sequential |
| `Stack(List(Sound))` | `stack(...)` | Parallel |
| `JuxRev(Sound)` | `.jux(rev)` | Stereo split with reverse |
| `Gain((Float, Sound))` | `.gain(n)` | Volume |
| `Pan((Float, Sound))` | `.pan(n)` | Stereo position |
| `Bank((String, Sound))` | `.bank(str)` | Sample bank |
| `Cpm((Float, Sound))` | `.cpm(n)` | Tempo (cycles per minute) |

---

## Phase 1: MVP - Get Sound Playing (COMPLETE)

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

## Phase 6: Strudel Parity Improvements

### 6.1 Live Coding Behavior (Priority: HIGH) ✅ COMPLETE

**Problem:** Calling `hush()` before every `play()` caused audio gaps when editing patterns.

**Solution:** Leverage Strudel's built-in `scheduler.setPattern()` which is called internally by `pattern.play()`. The key insight from [Strudel's REPL documentation](https://strudel.cc/technical-manual/repl/) is that `play()` updates the running scheduler without restarting the clock.

**Implementation (in `PlayerProj.re`):**

```reason
module PlayState = {
  let current: ref(option(Id.t)) = ref(Option.None);
  let last_desc: ref(string) = ref("");  // Track pattern changes

  let play_or_update = (id: Id.t, pattern: Strudel.pattern, desc: string) => {
    let is_new_pattern = last_desc^ != desc;
    let is_new_player = current^ != Option.Some(id);

    if (is_new_pattern || is_new_player) {
      // Only stop when switching players, not when updating same player
      if (is_new_player && current^ != Option.None) {
        Strudel.stopMusic();
      };
      // Don't call hush() - let scheduler.setPattern handle the update
      Strudel.playPattern(pattern);
      current := Option.Some(id);
      last_desc := desc;
    };
  };
  // ...
};
```

**Auto-update in view:** When a pattern is playing and its Sound expression changes, the view function automatically calls `play_or_update` to seamlessly update the pattern.

**Testing tips:**
- Use `Stack` to layer patterns (adding layers doesn't change timing structure)
- Change the `Slow` factor to hear tempo change mid-loop
- The "TOO LATE" console messages are normal Strudel scheduler behavior (events that couldn't be scheduled in time)

**Commit:** `c65b205b8`

### 6.2 Sample Loading & Graceful Degradation ✅ COMPLETE

**Current loading:** Samples load from `dough-samples` JSON manifests via Strudel's `samples()` function in the `prebake` callback:
1. `Dirt-Samples.json` - basic samples (bd, sd, hh, piano, etc.)
2. `tidal-drum-machines.json` - Roland drum machine banks (RolandTR808, RolandTR909, etc.)

**Implementation:** The `prebake` callback in `initStrudel()` is async with try/catch:
```javascript
prebake: async function() {
  var ds = 'https://raw.githubusercontent.com/felixroos/dough-samples/main/';
  try {
    await samples(ds + 'Dirt-Samples.json');
    await samples(ds + 'tidal-drum-machines.json');
  } catch (e) {
    console.warn('Strudel: Failed to load samples:', e);
  }
}
```

If network fails, samples won't load but:
- Strudel still initializes
- `Note()` patterns (synths) still work
- `Sample()` patterns play silence instead of crashing
- Console shows warning for debugging

**Future consideration:** Bundle a small starter kit (bd, sd, hh, cp, piano) for offline reliability.

### 6.3 Additional Sound Constructors (Priority: HIGH)

**Add to `BuiltinsADT.re` sound_type:**

```reason
let sound_type: Typ.t =
  sum_type([
    ("Note", Some(string())),
    ("Sample", Some(string())),
    ("Rev", Some(sound_inner)),
    ("Fast", Some(prod([float(), sound_inner]))),
    ("Slow", Some(prod([float(), sound_inner]))),
    ("Seq", Some(list(sound_inner))),
    ("Stack", Some(list(sound_inner))),
    // NEW:
    ("Gain", Some(prod([float(), sound_inner]))),
    ("Pan", Some(prod([float(), sound_inner]))),
    ("Bank", Some(prod([string(), sound_inner]))),
    ("JuxRev", Some(sound_inner)),  // jux(rev) - stereo widening
  ]);
```

**Add to `Strudel.re`:**

```reason
let gain: (float, pattern) => pattern =
  (g, p) =>
    Js.Unsafe.meth_call(p, "gain", [|Js.Unsafe.inject(Js.number_of_float(g))|]);

let pan: (float, pattern) => pattern =
  (n, p) =>
    Js.Unsafe.meth_call(p, "pan", [|Js.Unsafe.inject(Js.number_of_float(n))|]);

let bank: (string, pattern) => pattern =
  (name, p) =>
    Js.Unsafe.meth_call(p, "bank", [|Js.Unsafe.inject(Js.string(name))|]);
```

**Add to `SoundUtil.re` interpret_sound:**
```reason
| Some("Gain") =>
  switch (get_constructor_arg(exp)) {
  | Some(arg) =>
    switch (get_tuple(arg)) {
    | Some([gain_val, inner]) =>
      switch (get_float(gain_val), interpret_sound(inner)) {
      | (Some(g), Some(p)) => Some(Util.Strudel.gain(g, p))
      | _ => None
      }
    | _ => None
    }
  | None => None
  }
// Similar for Pan, Bank, JuxRev...
```

**Priority list:**

| Constructor | Type | Strudel | Status |
|-------------|------|---------|--------|
| `JuxRev` | `Sound` | `.jux(rev)` | ✅ DONE |
| `Gain` | `(Float, Sound)` | `.gain(n)` | ⬜ Phase A |
| `Pan` | `(Float, Sound)` | `.pan(n)` | ⬜ Phase A |
| `Bank` | `(String, Sound)` | `.bank(name)` | ⬜ Phase A |
| `Lpf` | `(Float, Sound)` | `.lpf(freq)` | ⬜ Later |
| `Hpf` | `(Float, Sound)` | `.hpf(freq)` | ⬜ Later |
| `Delay` | `(Float, Sound)` | `.delay(time)` | ⬜ Later |
| `Room` | `(Float, Sound)` | `.room(size)` | ⬜ Later |
| `Speed` | `(Float, Sound)` | `.speed(n)` | ⬜ Later |

### 6.4 Curried Standard Library (Priority: MEDIUM)

Add curried wrappers to Hazel's builtins or provide as a prelude:

```
// Full curried library (copy-pasteable):

// Base constructors
let note: String -> Sound = fun s -> Note(s) in
let sample: String -> Sound = fun s -> Sample(s) in

// Transformations (pattern-last for pipeline)
let rev: Sound -> Sound = fun s -> Rev(s) in
let fast: Float -> Sound -> Sound = fun f -> fun s -> Fast((f, s)) in
let slow: Float -> Sound -> Sound = fun f -> fun s -> Slow((f, s)) in
let gain: Float -> Sound -> Sound = fun g -> fun s -> Gain((g, s)) in
let pan: Float -> Sound -> Sound = fun p -> fun s -> Pan((p, s)) in
let bank: String -> Sound -> Sound = fun b -> fun s -> Bank((b, s)) in
let juxRev: Sound -> Sound = fun s -> JuxRev(s) in

// Composition
let seq: List(Sound) -> Sound = fun sounds -> Seq(sounds) in
let stack: List(Sound) -> Sound = fun sounds -> Stack(sounds) in

// Example: Full pipeline composition
let drums =
  sample("bd ~ sd ~")
  |> bank("RolandTR909")
  |> gain(0.8)
in
let melody =
  note("c4 e4 g4 c5")
  |> fast(2.0)
  |> juxRev
  |> gain(0.6)
in
stack([drums, melody])
```

**Implementation options:**
1. **User prelude:** Document and let users paste into their programs
2. **Built-in functions:** Add to `BuiltinsFunctions.re` as actual builtins
3. **Auto-include:** Automatically include in Sound-related example programs

### 6.5 Sample Exploration (Priority: MEDIUM)

**MVP (document + simple projector):**

Add to example programs or as a comment block:
```
// Available samples (from dirt-samples):
//
// DRUMS:
//   bd     - bass drum (variations: bd:0, bd:1, bd:2, ...)
//   sd     - snare drum
//   hh     - closed hi-hat
//   oh     - open hi-hat
//   cp     - clap
//   rim    - rimshot
//   tom    - tom drum
//   cb     - cowbell
//
// MELODIC:
//   piano  - acoustic piano
//   rhodes - electric piano
//   bass   - bass synth
//   strings - string ensemble
//
// BANKS (use with Bank constructor):
//   RolandTR808, RolandTR909, RolandTR707
//
// Mini-notation tips:
//   "bd*4"      - repeat 4 times
//   "hh/2"      - half speed
//   "[bd sd]"   - group (both in same time)
//   "<bd sd>"   - alternate each cycle
//   "hh:0 hh:1" - different sample variations
//   "bd ~ sd ~" - rests with ~
```

**Simple Sample Picker projector:**
- Triggers on `Sample(String)` where string is a single sample name
- Dropdown showing: bd, sd, hh, oh, cp, piano, etc.
- Selecting replaces the string

**Later: Full Sample Browser:**
- Category tree (Drums, Keys, Bass, etc.)
- Expand to show variations (hh:0, hh:1, hh:2)
- Click to audition (plays preview)
- Click to insert

### 6.6 Projector Visualization (Priority: MEDIUM)

**Phase A: Without source tracking (simpler)**

The RhythmGridProj already knows which step maps to which cell. For visualization:

1. Wire Strudel's `onTrigger` callback when playing
2. Callback receives timing info (beat fraction of cycle)
3. Map timing to grid step: `step = floor(fraction * num_steps)`
4. Highlight that cell

```reason
// Add to Strudel.re:
let with_on_trigger: (pattern, float => unit) => pattern =
  (p, callback) => {
    let js_callback = Js.wrap_callback((t, hap, _ct) => {
      // Extract begin time fraction from hap.whole.begin
      let begin_frac =
        Js.Unsafe.get(Js.Unsafe.get(hap, "whole"), "begin")
        |> Js.float_of_number;
      callback(begin_frac);
    });
    Js.Unsafe.meth_call(p, "onTrigger", [|Js.Unsafe.inject(js_callback)|]);
  };
```

**Incremental approach (onTrigger has caused issues before):**
1. First: Add `with_on_trigger` binding, test in console
2. Second: Wire to RhythmGridProj, just log to console
3. Third: Actually highlight cells
4. Test thoroughly at each step

**Phase B: With source tracking (complex, later)**

For highlighting Hazel source code (not just projector internals):
1. Thread source locations through `interpret_sound`
2. Store mapping: constructor ID → source span
3. Use `onTrigger` to get which pattern element triggered
4. Highlight corresponding Hazel source

This requires infrastructure for source location threading and overlay rendering.

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

### Medium Term - COMPLETE
8. ✅ Rhythm grid projector
9. ✅ UI polish: Block placeholders, contained black keys, symbol buttons
10. ✅ Context menu shows all applicable projectors (not just first)
11. ✅ Sample(String) constructor for drums/samples
12. ✅ Knob dial projector (modular synth style)
13. ✅ XY Pad projector (2D control surface)

### Completed - Strudel Parity
14. ✅ **Live coding behavior** - seamless pattern updates via scheduler.setPattern (see 6.1)
15. ✅ **Graceful sample degradation** - try/catch on dirt-samples loading (see 6.2)
16. ✅ **JuxRev constructor** - opt-in stereo widening (see 6.3)
17. ✅ **Playback cleanup** - removing Player stops audio

---

## Critical Gaps for Usability

### For Composing Music (Phase A - PRIORITY) ✅ COMPLETE

These features are essential to compose even simple pieces:

| Feature | Constructor | Strudel | Status |
|---------|-------------|---------|--------|
| **Volume control** | `Gain(Float, Sound)` | `.gain(n)` | ✅ Done |
| **Stereo panning** | `Pan(Float, Sound)` | `.pan(n)` | ✅ Done |
| **Sample banks** | `Bank(String, Sound)` | `.bank(name)` | ✅ Done |
| **Tempo control** | `Cpm(Float, Sound)` | `.cpm(n)` | ✅ Done |

**Implementation:** Added to `BuiltinsADT.re`, `SoundUtil.re`, and `Strudel.re`.

### For Live Coding Performance (Phase B - PRIORITY)

These features enable live performance workflows:

| Feature | Description | Status |
|---------|-------------|--------|
| **BPM/tempo control** | Set cycles per minute | ✅ Done via `Cpm(Float, Sound)` constructor |
| **Global transport** | Play/pause/stop all audio | ⬜ Deferred - need to decide on shortcut key |
| **Cycle indicator** | Show position in current cycle | ⬜ Future |

**Tempo Control:** Now handled via `Cpm(Float, Sound)` constructor. Users can:
- Use `Cpm((120.0, pattern))` for 120 CPM (≈ BPM)
- Apply Knob projector to the Float for visual control
- No global tempo UI needed - it's per-pattern

**Global Transport (DEFERRED):**
- Need to identify a keyboard shortcut that doesn't conflict with browser/OS
- Current Player toggle is `alt+p`
- Consider: Ctrl+., Cmd+Shift+Escape, or other combinations

---

## Implementation Phases

### Phase A: Music Composition Essentials (Do First) ✅ COMPLETE

**Goal:** Enable composing simple multi-layer pieces with proper mixing.

1. ✅ Add `Gain(Float, Sound)` constructor
2. ✅ Add `Pan(Float, Sound)` constructor
3. ✅ Add `Bank(String, Sound)` constructor
4. ✅ Add `Cpm(Float, Sound)` constructor for tempo control (cycles per minute ≈ BPM)
5. ⬜ Add sample documentation in example programs (deferred)

**Test case when complete:**
```
Stack([
  Gain((0.8, Bank(("RolandTR909", Sample("bd ~ ~ ~ bd ~ ~ ~"))))),
  Gain((0.6, Pan((0.3, Note("c4 ~ e4 ~"))))),
  Gain((0.4, Pan((-0.3, JuxRev(Note("g4 ~ ~ g4"))))))
])

# With tempo control:
Cpm((140.0, Stack([
  Gain((0.8, Bank(("RolandTR909", Sample("bd ~ sd ~"))))),
  Gain((0.6, Note("c4 e4 g4 c5")))
])))
```

### Phase B: Live Coding Essentials (Do Second)

**Goal:** Enable live performance with tempo control and global transport.

1. ✅ Tempo control via `Cpm(Float, Sound)` constructor - use Knob projector for visual editing
2. ⬜ **DEFERRED:** Global hush keyboard shortcut - need to decide on key that doesn't conflict with browser/OS shortcuts
3. ⬜ Consider: Global `setCps` binding for setting tempo across all patterns (different from per-pattern Cpm)
4. ⬜ Consider: Cycle position indicator (simpler than onTrigger visualization)

**Note on global shortcut:** The Player refractor currently uses `alt+p` to toggle. A global "stop all audio" shortcut needs careful consideration - Escape may conflict with dialog closing, and other keys may conflict with browser shortcuts. This is deferred until a suitable key combination is identified.

**Test case when complete:**
- ✅ Change tempo while pattern plays - use Cpm constructor with Knob projector
- ⬜ Press [TBD key] - all audio stops immediately
- ⬜ Multiple Players can be started, global stop affects all

### Phase C: Exploration & Polish (Later)

1. ⬜ Simple Sample Picker projector (dropdown)
2. ⬜ Scale picker projector
3. ⬜ Curried stdlib functions for pipeline style (see 6.4)
4. ⬜ Projector visualization Phase A (onTrigger, no source tracking)
5. ⬜ Player waveform visualization (Web Audio API oscilloscope/level meter)

### Long Term

1. ⬜ Full Sample Browser projector
2. ⬜ Projector visualization Phase B (source tracking)
3. ⬜ Euclidean rhythm projector
4. ⬜ Full piano keyboard projector (multi-octave)
5. ⬜ Additional effects: Lpf, Hpf, Delay, Room, Speed

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

## Phase 7: Special-Purpose Projector Design Principles

### 7.1 Core Principle: Strict at the Gate, Expressive Within

Special-purpose projectors (like NotePicker, RhythmGrid) provide visual affordances for editing a **subset** of what a general data type (like String) can represent. This creates a fundamental tension:

- **Too permissive:** Projector accepts patterns it can't faithfully edit → data loss, confusion
- **Too restrictive:** Projector rejects patterns it could partially handle → reduced utility

**The principle:** A projector should only be applicable when it can **faithfully represent and edit** the underlying data. Within that supported subset, the projector should be as expressive as possible.

### 7.2 Applicability Validation

Each special-purpose projector's `init` function should validate that the syntax matches its supported subset:

```reason
let init = (any: Language.Any.t) =>
  switch (string_of(any)) {
  | Some(s) when is_supported_pattern(s) => Some(initial_model)
  | _ => None  (* Not applicable - don't show in context menu *)
  };
```

**Validation should check:**
1. **Syntactic structure** - Is it the right constructor/type?
2. **Content constraints** - Does the string content match the supported subset?

**Example: NotePicker validation**
- ✅ `"c4 e4 g4"` - simple space-separated notes at same octave
- ✅ `"c#4 d4"` - sharps/flats supported
- ❌ `"c3 e4 g5"` - mixed octaves (can't represent on single-octave keyboard)
- ❌ `"c4*2 e4"` - repetition syntax not supported
- ❌ `"[c4 e4] g4"` - grouping syntax not supported

**Example: RhythmGrid validation**
- ✅ `"bd ~ sd ~"` - simple tokens and rests
- ❌ `"bd*4"` - repetition syntax not supported
- ❌ `"[bd sd]"` - grouping syntax not supported
- ❌ `"hh:0 hh:1"` - sample variations not supported

### 7.3 Current State: NotePicker

**Supported subset (after validation):**
- Space-separated note names with octave numbers
- All notes must be at the same octave
- Note format: `[a-g](#|b)?[0-9]` (e.g., `c4`, `d#4`, `eb4`)

**Current limitations:**
- Single octave display (detected from pattern, no manual selection)
- No rest (`~`) support
- No repetition, grouping, or alternation

**Roadmap for expressivity:**
- [x] Parse octaves from pattern, detect common octave
- [x] Display notes at detected octave on keyboard
- [x] Show full pattern text so all notes visible
- [ ] Add octave selector UI (+/- buttons or dropdown) for manual override
- [ ] Consider multi-octave keyboard display

### 7.4 Current State: RhythmGrid

**Supported subset (after validation):**
- Space-separated tokens
- Each token is either a drum name (`bd`, `sd`, `hh`, `oh`) or rest (`~`)
- No special syntax characters

**Current limitations:**
- Fixed set of drums (bd, sd, hh, oh)
- No repetition (`*n`) support
- No sample variations (`:n`)

**Roadmap for expressivity:**
- [ ] Support `*n` repetition by expanding to individual steps
- [ ] Support additional drum sounds (cp, rim, tom, cb)
- [ ] Consider sample variation display
- [ ] Variable step count persistence across pattern changes

### 7.5 Future: Disabled Projector Feedback

Currently, inapplicable projectors simply don't appear in the context menu. A more informative approach would be:

- Show inapplicable projectors grayed out in the menu
- Tooltip explains why it's not applicable (e.g., "NotePicker: pattern contains mixed octaves")
- Requires systematic changes to context menu infrastructure

This is deferred for now but noted as a potential UX improvement.

### 7.6 Guidelines for New Special-Purpose Projectors

When implementing a new projector that operates on a subset of a general type:

1. **Define the supported subset explicitly** - Document exactly what patterns are supported
2. **Validate in `init`** - Return `None` if pattern doesn't match supported subset
3. **Parse robustly** - Handle edge cases in the parsing logic
4. **Roundtrip faithfully** - `put(get(x)) == x` for all supported patterns
5. **Plan for expansion** - Note what features could be added in the roadmap

---

## Questions to Resolve

1. ✅ **Auto-play vs Click-to-play:** Require explicit play button (browser audio policy). First click enables audio context.

2. ✅ **Pattern Composition Model:** Both! Constructors (`Seq`, `Stack`) plus curried functions (`seq`, `stack`) for pipeline style. Strictly additive.

3. ✅ **Projector Activation:** Context menu option. User right-clicks and selects from applicable projectors.

4. ✅ **Real-time Updates:** Yes, immediate updates (like Strudel). See 6.1 for continuous scheduler implementation.

5. **Jux with functions:** Should `Jux` take a function argument (`Jux(Sound -> Sound, Sound)`) or just provide `JuxRev` as a special case? Function version requires higher-order Sound handling in interpreter.

6. **Sample bundling:** Should we bundle a small starter kit of samples for offline reliability, or rely entirely on GitHub loading with graceful degradation?

---

## References

- [Strudel Documentation](https://strudel.cc/)
- [Strudel JavaScript API](https://strudel.cc/functions/intro/)
- [Mini Notation Guide](https://strudel.cc/learn/mini-notation/)
- [Strudel Samples](https://strudel.cc/learn/samples/)
- [Visual Feedback](https://strudel.cc/learn/visual-feedback/)
- [Technical Manual - REPL](https://strudel.cc/technical-manual/repl/)
- [Technical Manual - Patterns](https://strudel.cc/technical-manual/patterns/)
- [Strudel Source (Codeberg)](https://codeberg.org/uzu/strudel)
- [TidalCycles](https://tidalcycles.org/)
- [onTrigger PR #136](https://codeberg.org/uzu/strudel/pulls/136)
