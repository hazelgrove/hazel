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
let window_chars = 66; /* 3 lines of 22 at 9 px inside the 170 px cloud */
let line_chars = 22;
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
/* set below: the placement loop and console testers */
let start_placement: ref(unit => unit) = ref(() => ());
/* ONE bubble, two skins. The sidebar says whether a thought is available
   (the model is streaming); the player says when a speech is up. Speech
   wins; both showing at once is not a state this can be in. */
let thought_available: ref(bool) = ref(false);
type mode =
  | Speech
  | Thought
  | Hidden;
let mode = (): mode =>
  saying() ? Speech : thought_available^ ? Thought : Hidden;
let mode_class = (m: mode): string =>
  switch (m) {
  | Speech => "mode-speech"
  | Thought => "mode-thought"
  | Hidden => "mode-hidden"
  };
let set_mode_class = (el, m: mode): unit => {
  let cl = Js.Unsafe.get(el, "classList");
  List.iter(
    c =>
      ignore(
        Js.Unsafe.meth_call(
          cl,
          "remove",
          [|Js.Unsafe.inject(Js.string(c))|],
        ),
      ),
    ["mode-speech", "mode-thought", "mode-hidden"],
  );
  ignore(
    Js.Unsafe.meth_call(
      cl,
      "add",
      [|Js.Unsafe.inject(Js.string(mode_class(m)))|],
    ),
  );
};

let rec tick = (): unit => {
  switch (el_by_class("canvas-avatar-bubble")) {
  | Some(bub) =>
    let m = mode();
    set_mode_class(bub, m);
    switch (m) {
    | Speech =>
      switch (el_by_class("say-text")) {
      | Some(t) => set_text(t, say_text^)
      | None => ()
      }
    | Thought =>
      switch (el_by_class("bubble-text")) {
      | Some(el) =>
        let w = clean_cut(advance());
        set_text(el, w);
        /* a fade on each replacement, so the change reads as motion */
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
      }
    | Hidden => ()
    };
  | None => ()
  };
  ignore(Js.Unsafe.global##setTimeout(Js.Unsafe.callback(tick), tick_ms));
};
let ensure_loop = (): unit =>
  if (! running^) {
    running := true;
    tick();
    start_placement^();
  };

/* ---- placement: the bubble emanates from the avatar's bounding circle ----
   The speech tail's tip, or the cloud's last puff, sits ON the circle; the
   bubble body lies further out along the same direction. Default: the
   upper-left quadrant; near the pane's top or left the direction flips so
   the bubble stays in view. Written every frame in body-local px (the
   bubble is a child of the body). The speech skin is ONE svg path: a
   rounded box whose near corner becomes the tail. */

let force_cloud: ref(bool) = ref(false);

let set_style = (el, name: string, v: string): unit =>
  ignore(
    Js.Unsafe.meth_call(
      Js.Unsafe.get(el, "style"),
      "setProperty",
      [|
        Js.Unsafe.inject(Js.string(name)),
        Js.Unsafe.inject(Js.string(v)),
      |],
    ),
  );
let set_attr = (el, name: string, v: string): unit =>
  ignore(
    Js.Unsafe.meth_call(
      el,
      "setAttribute",
      [|
        Js.Unsafe.inject(Js.string(name)),
        Js.Unsafe.inject(Js.string(v)),
      |],
    ),
  );
let f1 = (x: float) => Printf.sprintf("%.1f", x);
let child = (b, sel: string) =>
  Js.Opt.to_option(
    Js.Unsafe.meth_call(
      b,
      "querySelector",
      [|Js.Unsafe.inject(Js.string(sel))|],
    ),
  );

/* the speech outline in bubble-local px: a w×h box with radius r on
   three corners; the near corner (toward the avatar, by the signs of the
   direction) is replaced by the tail out to (tx, ty) */
let speech_path =
    (~w: float, ~h: float, ~sx: float, ~sy: float, (tx, ty): (float, float))
    : string => {
  let r = 9.;
  /* clockwise: TL, TR, BR, BL with their incoming/outgoing directions */
  let corners = [|
    ((0., 0.), (0., (-1.)), (1., 0.)),
    ((w, 0.), (1., 0.), (0., 1.)),
    ((w, h), (0., 1.), ((-1.), 0.)),
    ((0., h), ((-1.), 0.), (0., (-1.))),
  |];
  let tail =
    switch (sx < 0., sy < 0.) {
    | (true, true) => 2 /* bubble up-left of the avatar: tail at BR */
    | (false, true) => 3 /* up-right: BL */
    | (true, false) => 1 /* down-left: TR */
    | (false, false) => 0 /* down-right: TL */
    };
  let pt = ((x, y)) => f1(x) ++ " " ++ f1(y);
  /* the tail's base spans 13 px back along the incoming edge and 11 px on
     along the outgoing one, but never past the neighbouring arcs on a
     short edge (a one-line bubble is ~17 px tall) */
  let edge_len = ((ix, iy)) => Float.abs(ix) > 0.5 ? w : h;
  let segs =
    Array.to_list(
      Array.mapi(
        (k, ((cx, cy), (ix, iy), (ox, oy))) =>
          if (k == tail) {
            let back = min(13., 0.45 *. edge_len((ix, iy)))
            and on = min(11., 0.45 *. edge_len((ox, oy)));
            Printf.sprintf(
              "L %s L %s L %s",
              pt((cx -. ix *. back, cy -. iy *. back)),
              pt((tx, ty)),
              pt((cx +. ox *. on, cy +. oy *. on)),
            );
          } else {
            Printf.sprintf(
              "L %s Q %s %s",
              pt((cx -. ix *. r, cy -. iy *. r)),
              pt((cx, cy)),
              pt((cx +. ox *. r, cy +. oy *. r)),
            );
          },
        corners,
      ),
    );
  /* start on the top edge just past TL (or past TL's tail base) */
  let start = tail == 0 ? pt((min(11., 0.45 *. w), 0.)) : pt((r, 0.));
  "M "
  ++ start
  ++ " "
  ++ String.concat(" ", List.tl(segs))
  ++ " "
  ++ List.hd(segs)
  ++ " Z";
};

let place = (): unit =>
  switch (CanvasAvatar.body()) {
  | None => ()
  | Some(b) =>
    switch (child(b, ".canvas-avatar-bubble")) {
    | None => ()
    | Some(bub) =>
      let m = mode();
      let (cx, cy, r) = CanvasAvatar.bounding_circle();
      /* which way is there room: the circle's position in the pane */
      let zoom = max(0.2, CanvasBuffer.canvas_zoom^);
      let (sx, sy) =
        switch (Util.JsUtil.get_elem_by_id_opt("canvas-scroll")) {
        | Some(pane) =>
          let pr = Js.Unsafe.meth_call(pane, "getBoundingClientRect", [||])
          and br = Js.Unsafe.meth_call(b, "getBoundingClientRect", [||]);
          let px: float = Js.Unsafe.get(pr, "left")
          and py: float = Js.Unsafe.get(pr, "top")
          and pw: float = Js.Unsafe.get(pr, "width")
          and ph: float = Js.Unsafe.get(pr, "height");
          let bx: float = Js.Unsafe.get(br, "left") +. cx *. zoom
          and by: float = Js.Unsafe.get(br, "top") +. cy *. zoom;
          let fx = (bx -. px) /. max(1., pw)
          and fy = (by -. py) /. max(1., ph);
          (fx > 0.38 ? (-1.) : 1., fy > 0.3 ? (-1.) : 1.);
        | None => ((-1.), (-1.))
        };
      let d = 0.7071;
      let dx = sx *. d
      and dy = sy *. d;
      let tipx = cx +. dx *. r
      and tipy = cy +. dy *. r;
      /* the bubble box: its near corner at gap g beyond the tip */
      let g =
        switch (m) {
        | Speech => 9.
        | Thought
        | Hidden => 13.
        };
      let w: float = Js.Unsafe.get(bub, "offsetWidth")
      and h: float = Js.Unsafe.get(bub, "offsetHeight");
      let kx = tipx +. dx *. g
      and ky = tipy +. dy *. g;
      let left = sx < 0. ? kx -. w : kx
      and top = sy < 0. ? ky -. h : ky;
      set_style(bub, "left", f1(left) ++ "px");
      set_style(bub, "top", f1(top) ++ "px");
      /* the speech skin's outline, tail included */
      switch (child(bub, ".say-shape path")) {
      | Some(path) when m == Speech =>
        set_attr(
          path,
          "d",
          speech_path(~w, ~h, ~sx, ~sy, (tipx -. left, tipy -. top)),
        )
      | _ => ()
      };
      /* the cloud's puffs: small, a little space, larger, a little space */
      switch (child(b, ".bubble-tails")) {
      | Some(t) =>
        List.iter(
          ((sel, along, rad)) =>
            switch (child(t, sel)) {
            | Some(c) =>
              set_attr(c, "cx", f1(tipx +. dx *. along));
              set_attr(c, "cy", f1(tipy +. dy *. along));
              set_attr(c, "r", f1(rad));
              set_attr(c, "opacity", m == Thought ? "1" : "0");
            | None => ()
            },
          [(".cloud-puff-1", 2.8, 2.6), (".cloud-puff-2", 10.6, 4.2)],
        )
      | None => ()
      };
    }
  };

let placing: ref(bool) = ref(false);
let rec place_loop = (_: float): unit => {
  place();
  ignore(
    Js.Unsafe.global##requestAnimationFrame(Js.Unsafe.callback(place_loop)),
  );
};
let ensure_place_loop = (): unit =>
  if (! placing^) {
    placing := true;
    place_loop(0.);
  };

let install_testers = (): unit => {
  let g = Js.Unsafe.global;
  if (!Js.Optdef.test(Js.Unsafe.get(g, "__bubbleDemo"))) {
    Js.Unsafe.set(
      g,
      "__bubbleDemo",
      Js.Unsafe.callback((text: Js.t(Js.js_string)) => {
        source := Js.to_string(text);
        pos := 0;
        force_cloud := true;
        CanvasTrajectory.on_change^();
      }),
    );
    Js.Unsafe.set(
      g,
      "__bubbleDemoOff",
      Js.Unsafe.callback(() => {
        force_cloud := false;
        CanvasTrajectory.on_change^();
      }),
    );
    Js.Unsafe.set(
      g,
      "__bubbleSay",
      Js.Unsafe.callback((text: Js.t(Js.js_string), ms: float) =>
        say(Js.to_string(text), ~ms)
      ),
    );
  };
};

start_placement :=
  (
    () => {
      ensure_place_loop();
      install_testers();
    }
  );
