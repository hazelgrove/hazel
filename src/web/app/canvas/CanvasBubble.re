/* CanvasBubble — the agent's two bubbles, driven outside the vdom.

   THOUGHT: while the model streams, a cloud beside the avatar shows a
   window of the streamed reasoning, replaced by the next window on a
   quick cadence (~3.5 Hz) so the text flows through it — the impression
   of thinking, not reading material (words can be caught). The sidebar
   hands the full streamed text to [source] each render; the loop writes
   the window into the cloud's text element, which the view renders empty.

   SPEECH: when an act lands, a small speech bubble at the same anchor
   calls out the action (the score's cause and site), for the act's
   duration. The player calls [say]. */

open Js_of_ocaml;

let source: ref(string) = ref("");
/* the window's start into the source, advanced each tick */
let pos: ref(int) = ref(0);
let window_chars = 84; /* ~3 lines of 28 at 9 px in a 150 px cloud */
let line_chars = 28;
let tick_ms = 290.;

let el_by_class = (cls: string) =>
  Js.Opt.to_option(
    Js.Unsafe.meth_call(
      Js.Unsafe.global##.document,
      "querySelector",
      [|Js.Unsafe.inject(Js.string("." ++ cls))|],
    ),
  );

let set_text = (el, s: string): unit =>
  Js.Unsafe.set(el, "textContent", Js.string(s));

/* the next window: one line further, catching up when the stream ran
   ahead; at the end the window stays (fresh text arrives at its tail) */
let advance = (): string => {
  let n = String.length(source^);
  if (n <= window_chars) {
    pos := 0;
    source^;
  } else {
    let last_start = n - window_chars;
    let next = pos^ + line_chars;
    pos := next > last_start ? last_start : next;
    /* fell far behind: jump */
    if (last_start - pos^ > 3 * window_chars) {
      pos := last_start - window_chars;
    };
    String.sub(source^, pos^, window_chars);
  };
};

/* utf-8: never cut inside a multi-byte sequence */
let clean_cut = (s: string): string => {
  let n = String.length(s);
  let rec trim_head = i =>
    if (i < n && Char.code(s.[i]) land 0xC0 == 0x80) {
      trim_head(i + 1);
    } else {
      i;
    };
  let h = trim_head(0);
  let rec trim_tail = j =>
    if (j > h && Char.code(s.[j - 1]) land 0xC0 == 0x80) {
      trim_tail(j - 1);
    } else if (j > h && Char.code(s.[j - 1]) land 0xC0 == 0xC0) {
      j - 1;
    } else {
      j;
    };
  let t = trim_tail(n);
  t > h ? String.sub(s, h, t - h) : "";
};

/* ---- speech ---- */
let say_text: ref(string) = ref("");
let say_until: ref(float) = ref(0.);
let now = (): float => Js.Unsafe.coerce(Js.Unsafe.global)##._Date##now();
let say = (text: string, ~ms: float): unit => {
  say_text := text;
  say_until := now() +. ms;
};
let saying = (): bool => now() < say_until^;

let running: ref(bool) = ref(false);
let rec tick = (): unit => {
  switch (el_by_class("bubble-text")) {
  | Some(el) =>
    let w = clean_cut(advance());
    set_text(el, w);
    /* a beat on each replacement, so the change reads as motion */
    let cl = Js.Unsafe.get(el, "classList");
    ignore(
      Js.Unsafe.meth_call(
        cl,
        "remove",
        [|Js.Unsafe.inject(Js.string("flip"))|],
      ),
    );
    ignore(Js.Unsafe.get(el, "offsetWidth")); /* restart the animation */
    ignore(
      Js.Unsafe.meth_call(
        cl,
        "add",
        [|Js.Unsafe.inject(Js.string("flip"))|],
      ),
    );
  | None => ()
  };
  switch (el_by_class("canvas-avatar-say")) {
  | Some(el) =>
    let on = saying();
    let cl = Js.Unsafe.get(el, "classList");
    ignore(
      Js.Unsafe.meth_call(
        cl,
        on ? "add" : "remove",
        [|Js.Unsafe.inject(Js.string("say-on"))|],
      ),
    );
    if (on) {
      switch (el_by_class("say-text")) {
      | Some(t) => set_text(t, say_text^)
      | None => ()
      };
    };
  | None => ()
  };
  ignore(Js.Unsafe.global##setTimeout(Js.Unsafe.callback(tick), tick_ms));
};
let ensure_loop = (): unit =>
  if (! running^) {
    running := true;
    tick();
  };
