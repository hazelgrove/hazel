open Js_of_ocaml;

/* Check if a window property is a function */
let is_function = (name: string): bool => {
  Js.Unsafe.fun_call(
    Js.Unsafe.js_expr(
      "(function(n) { return typeof window[n] === 'function'; })",
    ),
    [|Js.Unsafe.inject(Js.string(name))|],
  )
  |> Js.to_bool;
};

/* Check if Strudel is ready (note function exists) */
let isReady: unit => bool = () => is_function("note");

/* Safe init - only call if initStrudel exists */
/* Pass prebake callback to load samples from GitHub with error handling.
 * Loads both dirt-samples (basic sounds) and tidal-drum-machines (Roland banks). */
let initStrudel: unit => unit =
  () =>
    if (is_function("initStrudel")) {
      let fn = Js.Unsafe.js_expr("window.initStrudel");
      /* Create options object with prebake callback that loads samples.
       * Wrapped in try/catch for graceful degradation on network failure.
       * Uses dough-samples JSON manifests which include both dirt-samples and drum-machines. */
      let options =
        Js.Unsafe.js_expr(
          "{ prebake: async function() { \
             var ds = 'https://raw.githubusercontent.com/felixroos/dough-samples/main/'; \
             try { \
               await samples(ds + 'Dirt-Samples.json'); \
               await samples(ds + 'tidal-drum-machines.json'); \
             } catch (e) { \
               console.warn('Strudel: Failed to load samples:', e); \
             } \
           } }",
        );
      Js.Unsafe.fun_call(fn, [|Js.Unsafe.inject(options)|]) |> ignore;
    };

/* Safe hush - only call if hush exists */
let hush: unit => unit =
  () =>
    if (is_function("hush")) {
      let fn = Js.Unsafe.js_expr("window.hush");
      Js.Unsafe.fun_call(fn, [||]) |> ignore;
    };

/* Abstract type for Strudel patterns */
type pattern;

/* Create a note pattern from mini-notation string (synth tones) */
let note: string => option(pattern) =
  s =>
    if (isReady()) {
      let noteFn = Js.Unsafe.js_expr("window.note");
      Some(
        Js.Unsafe.fun_call(noteFn, [|Js.Unsafe.inject(Js.string(s))|]),
      );
    } else {
      None;
    };

/* Create a sample pattern from mini-notation string (drums, synths, etc) */
let sound: string => option(pattern) =
  s =>
    if (is_function("s")) {
      let soundFn = Js.Unsafe.js_expr("window.s");
      Some(
        Js.Unsafe.fun_call(soundFn, [|Js.Unsafe.inject(Js.string(s))|]),
      );
    } else {
      None;
    };

/* Reverse a pattern */
let rev: pattern => pattern = p => Js.Unsafe.meth_call(p, "rev", [||]);

/* Speed up a pattern */
let fast: (float, pattern) => pattern =
  (f, p) =>
    Js.Unsafe.meth_call(
      p,
      "fast",
      [|Js.Unsafe.inject(Js.number_of_float(f))|],
    );

/* Slow down a pattern */
let slow: (float, pattern) => pattern =
  (f, p) =>
    Js.Unsafe.meth_call(
      p,
      "slow",
      [|Js.Unsafe.inject(Js.number_of_float(f))|],
    );

/* Stack patterns (play simultaneously) */
let stack: list(pattern) => pattern =
  patterns => {
    let stackFn = Js.Unsafe.js_expr("window.stack");
    let arr = Js.array(Array.of_list(patterns));
    Js.Unsafe.fun_call(stackFn, [|Js.Unsafe.inject(arr)|]);
  };

/* Sequence patterns (play one after another) */
let seq: list(pattern) => pattern =
  patterns => {
    let seqFn = Js.Unsafe.js_expr("window.seq");
    let arr = Js.array(Array.of_list(patterns));
    Js.Unsafe.fun_call(seqFn, [|Js.Unsafe.inject(arr)|]);
  };

/* Apply jux with rev for stereo effect */
let juxRev: pattern => pattern =
  p => {
    let revFn = Js.Unsafe.js_expr("window.rev");
    Js.Unsafe.meth_call(p, "jux", [|Js.Unsafe.inject(revFn)|]);
  };

/* Set gain/volume (0.0 - 1.0) */
let gain: (float, pattern) => pattern =
  (g, p) =>
    Js.Unsafe.meth_call(
      p,
      "gain",
      [|Js.Unsafe.inject(Js.number_of_float(g))|],
    );

/* Set stereo pan (-1.0 left to 1.0 right) */
let pan: (float, pattern) => pattern =
  (n, p) =>
    Js.Unsafe.meth_call(
      p,
      "pan",
      [|Js.Unsafe.inject(Js.number_of_float(n))|],
    );

/* Set sample bank (e.g., "RolandTR909") */
let bank: (string, pattern) => pattern =
  (name, p) =>
    Js.Unsafe.meth_call(p, "bank", [|Js.Unsafe.inject(Js.string(name))|]);

/* Set tempo in cycles per minute (similar to BPM) */
let cpm: (float, pattern) => pattern =
  (n, p) =>
    Js.Unsafe.meth_call(
      p,
      "cpm",
      [|Js.Unsafe.inject(Js.number_of_float(n))|],
    );

/* Play a pattern */
let play: pattern => unit =
  p => Js.Unsafe.meth_call(p, "play", [||]) |> ignore;

/* Play a note pattern - fully defensive (legacy interface) */
let playNote: string => unit =
  pattern =>
    switch (note(pattern)) {
    | Some(p) => play(p)
    | None => ()
    };

/* Play an arbitrary pattern */
let playPattern: pattern => unit = p => play(p);

/* Function to stop the music */
let stopMusic: unit => unit = () => hush();

/* Global play state for mutual exclusion - only one Player can be active at a time.
 * For live coding: we don't call hush() before playing - this allows the
 * Strudel scheduler to keep running while we update the pattern via setPattern.
 * See: https://strudel.cc/technical-manual/repl/ */
module PlayState = {
  /* Use Uuidm.t directly since util doesn't depend on Language.Id */
  type player_id = Uuidm.t;

  let current: ref(option(player_id)) = ref(Option.None);
  /* Track last played description to detect actual changes */
  let last_desc: ref(string) = ref("");

  /* Start playing or update the pattern if already playing.
   * Key insight: pattern.play() internally calls scheduler.setPattern(),
   * which updates the running scheduler without restarting the clock. */
  let play_or_update = (id: player_id, p: pattern, desc: string) => {
    let is_new_pattern = last_desc^ != desc;
    let is_new_player = current^ != Option.Some(id);

    if (is_new_pattern || is_new_player) {
      /* If switching to a different player, stop the previous one first */
      if (is_new_player && current^ != Option.None) {
        stopMusic();
      };
      /* Don't call hush() here - let scheduler.setPattern handle the update */
      playPattern(p);
      current := Option.Some(id);
      last_desc := desc;
    };
  };

  let stop = () => {
    stopMusic();
    current := Option.None;
    last_desc := "";
  };

  let is_playing = (id: player_id) => current^ == Option.Some(id);

  /* Stop playback if any of the given IDs is the currently playing player */
  let stop_if_playing_any = (ids: list(player_id)) =>
    switch (current^) {
    | Some(playing_id) when List.mem(playing_id, ids) => stop()
    | _ => ()
    };
};

/* Function to initialize Strudel - handles both already-loaded and loading cases */
let initOnLoad: unit => unit =
  () => {
    /* Check if initStrudel exists and call it */
    let doInit = () =>
      if (is_function("initStrudel")) {
        initStrudel();
        Printf.printf(
          "Strudel initialized with Dirt-Samples, note ready: %b\n",
          isReady(),
        );
      } else {
        Printf.printf("Strudel initStrudel function not found\n");
      };
    /* Check if DOM is already loaded */
    let readyState =
      Js.Unsafe.get(Js.Unsafe.js_expr("document"), "readyState")
      |> Js.to_string;
    if (readyState == "complete" || readyState == "interactive") {
      /* DOM already loaded, init immediately */
      doInit();
    } else {
      /* Wait for DOMContentLoaded */
      let cb = Js.wrap_callback(_ => doInit());
      Js.Unsafe.fun_call(
        Js.Unsafe.js_expr("window.addEventListener"),
        [|
          Js.Unsafe.inject(Js.string("DOMContentLoaded")),
          Js.Unsafe.inject(cb),
        |],
      )
      |> ignore;
    };
  };
