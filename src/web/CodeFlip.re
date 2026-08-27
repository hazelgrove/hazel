open Util;
open Js_of_ocaml;
open Haz3lcore;

/* FLIP for code movement: animates the REAL token elements in place.
 * request() (during the MVU update, before the edit applies) captures
 * the pre-edit Measured map. go() (after render) walks the post-edit
 * segment in emission order to pair each leaf with its DOM node in
 * .code-text — Code.view emits children in segment order, so no ids
 * are needed — then WAAPI-translates every moved element from its old
 * grid position to rest. The element that lands is the element that
 * flies: no overlay copies, hence no double-image and nothing to
 * suppress, and styling/decoration context is automatic. Positions
 * come from diffing Measured maps — pure arithmetic, no layout reads.
 * Companion to Animation.re (the caret's DOM-box FLIP). */

/* Slightly statelier than the caret's 125ms (code moves farther) */
let duration = 180;
let easing = Animation.easeOutExpo;

/* debug slow-motion: 5x all timed code animations (flights, enters,
   rebounds, relaxes — scrubs are pointer-driven and unaffected).
   Flipped from the command palette; deliberately NOT a settings
   field (adding one resets persisted settings). */
let slow_mo: ref(bool) = ref(false);
let dur = (ms: int): int => slow_mo^ ? ms * 5 : ms;

/* perf guard: a transition that moves this much is better skipped */
let max_moved = 1500;

type key =
  | Shard(Id.t, int)
  | GroutK(Id.t)
  | CommentK(Id.t);

/* Expected .code-text children, mirroring Code.view's emission */
type entry =
  | Elem(key) /* an element node we may animate */
  | Txt /* a text node (whitespace); nothing visible to animate */
  | Opaque /* projector: element or text; consume without animating */;

let rec entries_of_segment = (seg: Segment.t): list(entry) =>
  seg
  |> List.concat_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         Aba.mk(t.shards, t.children)
         |> Aba.join(i => [Elem(Shard(t.id, i))], entries_of_segment)
         |> List.concat
       | Grout(g) => [Elem(GroutK(g.id))]
       | Secondary(s) =>
         switch (s.content) {
         | Comment(_) => [Elem(CommentK(s.id))]
         | Whitespace(_) => [Txt]
         }
       | Projector(_) => [Opaque]
       }
     );

let find_meas = (m: Measured.t, k: key): option(Measured.measurement) =>
  switch (k) {
  | Shard(id, i) =>
    switch (Id.Map.find_opt(id, m.tiles)) {
    | Some(shards) => List.assoc_opt(i, shards)
    | None => None
    }
  | GroutK(id) => Id.Map.find_opt(id, m.grout)
  | CommentK(id) => Id.Map.find_opt(id, m.secondary)
  };

/* a tile's label, found anywhere in a segment */
let rec tile_label = (seg: Segment.t, id: Id.t): option(list(string)) =>
  seg
  |> List.find_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         t.id == id
           ? Some(t.label)
           : t.children |> List.find_map(c => tile_label(c, id))
       | _ => None
       }
     );

/* lookup with END-ALIGNED closing delimiters: a tile whose shard
   count changed (cons-split drops a list comma) pairs naturally by
   index EXCEPT its closing delimiter — `]` must fly from `]`, not
   from the old last-comma slot. Interior separators are identical
   glyphs, so from-start alignment is invisible for them. k is
   indexed by `other`'s shard count; the lookup happens in `m`.
   GUARD (andrew's typing-a-let bug): end-alignment only holds when
   the two last shards are the SAME DELIMITER TEXT — a tile GROWING
   (`let x` gaining its `=`, then its `in`) also changes its last
   index, and blind remapping flew each new delimiter in from the
   previous one (and back, on deletion). With segments provided the
   glyphs gate the remap; a new distinct delimiter gets no old
   counterpart and correctly enters instead of flying. */
let find_meas_end_aligned =
    (
      ~m_seg: option(Segment.t)=?,
      ~other_seg: option(Segment.t)=?,
      ~other: Measured.t,
      m: Measured.t,
      k: key,
    )
    : option(Measured.measurement) =>
  switch (k) {
  | Shard(id, i) =>
    switch (Id.Map.find_opt(id, m.tiles), Id.Map.find_opt(id, other.tiles)) {
    | (Some(m_shards), Some(o_shards)) =>
      let last = shards => shards |> List.map(fst) |> List.fold_left(max, 0);
      let (m_last, o_last) = (last(m_shards), last(o_shards));
      let same_glyph =
        switch (m_seg, other_seg) {
        | (Some(ms), Some(os)) =>
          switch (tile_label(ms, id), tile_label(os, id)) {
          | (Some(ml), Some(ol)) =>
            switch (List.nth_opt(ml, m_last), List.nth_opt(ol, o_last)) {
            | (Some(a), Some(b)) => a == b
            | _ => false
            }
          | _ => false
          }
        /* no syntax in hand (drag candidates): legacy behavior */
        | _ => true
        };
      let i' = m_last != o_last && i == o_last && same_glyph ? m_last : i;
      List.assoc_opt(i', m_shards);
    | _ => find_meas(m, k)
    }
  | _ => find_meas(m, k)
  };

/* Pair entries with .code-text's childNodes; None on any mismatch
 * (bail out, skip animation — never guess at correlation) */
let pair =
    (entries: list(entry), nodes: list(Js.t(Dom.node)))
    : option(list((key, Js.t(Dom.node)))) => {
  let rec walk = (entries, nodes, acc) =>
    switch (entries, nodes) {
    | ([], []) => Some(acc)
    /* Code.view appends a zero-width-space TEXT node as a trailing
       line-box filler (unconditionally) — tolerate it, or every
       pairing fails and all token animation silently dies (it did,
       from the moment that filler merged in upstream) */
    | ([], [n]) =>
      switch (n##.nodeType) {
      | Dom.TEXT => Some(acc)
      | _ => None
      }
    | ([Txt, ...es], [n, ...ns]) =>
      switch (n##.nodeType) {
      | Dom.TEXT => walk(es, ns, acc)
      | _ => None
      }
    | ([Opaque, ...es], [_, ...ns]) => walk(es, ns, acc)
    | ([Elem(k), ...es], [n, ...ns]) =>
      switch (n##.nodeType) {
      | Dom.ELEMENT => walk(es, ns, [(k, n), ...acc])
      | _ => None
      }
    | _ => None
    };
  walk(entries, nodes, []);
};

/* Animations in flight, so a new batch can cancel stragglers.
 * Note we never touch element styles: tokens are display:inline-block
 * via the stylesheet (transforms are ignored on plain inline boxes),
 * and inline-style mutations get clobbered by later vdom patches. */
let active: ref(list(Js.Unsafe.any)) = ref([]);

/* flip telemetry: every render and flight outcome journals into
   window.__flipLog (ring, last 1000). Inspect raw, or summarize:
     __flipLog.filter(e => e.k == "end").map(e => e.how)
   Exists because "did the animation play" is unanswerable by eye at
   125ms — skips have four distinct causes (cancelled by the next
   batch, node replaced by a vdom patch, pairing bailed, cap) and
   only a journal tells them apart. Negligible cost. */
let flip_log = (fields: list((string, Js.Unsafe.any))): unit =>
  try({
    let push =
      Js.Unsafe.js_expr(
        "(function(e){var l=(window.__flipLog=window.__flipLog||[]);l.push(e);if(l.length>1000)l.shift();})",
      );
    let now = Js.Unsafe.js_expr("performance.now()");
    let obj = Js.Unsafe.obj(Array.of_list([("t", now), ...fields]));
    Js.Unsafe.fun_call(push, [|obj|]) |> ignore;
  }) {
  | _ => ()
  };
let str = (x: string): Js.Unsafe.any => Js.Unsafe.inject(Js.string(x));
let num = (x: float): Js.Unsafe.any =>
  Js.Unsafe.inject(Js.number_of_float(x));
let int_ = (x: int): Js.Unsafe.any => num(float_of_int(x));

/* watch a created animation's fate; conn records whether the node
   was still in the document at end time — a "finish" with
   conn=false is the silent kill (a vdom patch replaced the node;
   nothing was on screen) */
let watch_anim =
    (kind: string, node: Js.t(Dom.node), anim: Js.Unsafe.any): unit =>
  try({
    let t0 = Js.Unsafe.eval_string("performance.now()");
    let mk = how =>
      Js.wrap_callback(_ =>
        flip_log([
          ("k", str("end")),
          ("kind", str(kind)),
          ("how", str(how)),
          (
            "age",
            Js.Unsafe.js_expr("performance.now()") |> Js.Unsafe.inject,
          ),
          ("t0", t0),
          ("conn", Js.Unsafe.inject(Js.Unsafe.get(node, "isConnected"))),
        ])
      );
    Js.Unsafe.set(anim, "onfinish", mk("finish"));
    Js.Unsafe.set(anim, "oncancel", mk("cancel"));
  }) {
  | _ => ()
  };

/* mergeInto targets (D2 emerge REVERSED — emergeMode=clone is
   bidirectional in dragology's lerp; our exits leave the DOM, so the
   reverse direction runs on synthetic ghosts): the dissolved
   window's ids (old segment) and the surviving window's ids (live).
   Ghost copies of the dissolved tokens converge onto the survivor at
   full opacity and are removed on arrival — landing on identical
   text, the removal is invisible. */
let merge_staged: ref((list(Id.t), list(Id.t))) = ref(([], []));
let set_merge = ((dissolved, survivor): (list(Id.t), list(Id.t))): unit =>
  merge_staged := (dissolved, survivor);

let merge_overlay_id = "code-flip-merge-overlay";
let remove_merge_overlay = (): unit =>
  switch (
    Js.Opt.to_option(
      Dom_html.document##getElementById(Js.string(merge_overlay_id)),
    )
  ) {
  | Some(el) => Js.Opt.iter(el##.parentNode, p => Dom.removeChild(p, el))
  | None => ()
  };

let cancel_active = (): unit => {
  active^
  |> List.iter(anim =>
       switch (Js.Unsafe.meth_call(anim, "cancel", [||])) {
       | exception _ => ()
       | _ => ()
       }
     );
  active := [];
  remove_merge_overlay();
};

/* EXPERIMENT (andrew): newly created elements fade in (they used to
 * pop). Exit animation is structurally out of reach here: removed
 * elements are already gone from the DOM when go() runs. */
let enter_duration = 320; /* slower than movement so it registers */

/* drag handoff: the ghost previewed the entrance at progress = pull
   t — the real tokens CONTINUE from there, same continuation rule as
   movement (grow-ins continue opacity+scale; emerge flights continue
   position) */
let drag_enter_from: ref(option(float)) = ref(None);
let set_drag_enter = (t: float): unit => drag_enter_from := Some(t);

/* emergeFrom sources (D2 emergeMode=clone): the LIVE ids of the def
   subtree a feed clones. Consumed by go(): entered tokens are zipped
   POSITIONALLY against these keys in the old segment (fresh clone ids
   are minted per prepare run — id-keyed correlation silently misses
   across the speculative/commit boundary; position + kind is the
   stable correlate). Matched enters FLY full-size from the source —
   a split, not a growth. */
let emerge_src: ref(list(Id.t)) = ref([]);
let set_emerge_src = (ids: list(Id.t)): unit => emerge_src := ids;

let key_kind_match = (a: key, b: key): bool =>
  switch (a, b) {
  | (Shard(_, i), Shard(_, j)) => i == j
  | (GroutK(_), GroutK(_))
  | (CommentK(_), CommentK(_)) => true
  | _ => false
  };

/* (key, token text) for the given ids' tokens, in traversal order —
   the emerge source's identity card (text disambiguates windows;
   grout carries none = wildcard) */
let rec keyed_tokens_of_segment =
        (ids: list(Id.t), seg: Segment.t): list((key, string)) =>
  seg
  |> List.concat_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         Aba.mk(t.shards, t.children)
         |> Aba.join(
              i =>
                List.mem(t.id, ids)
                  ? [
                    (
                      Shard(t.id, i),
                      List.nth_opt(t.label, i) |> Option.value(~default=""),
                    ),
                  ]
                  : [],
              keyed_tokens_of_segment(ids),
            )
         |> List.concat
       | Grout(g) => List.mem(g.id, ids) ? [(GroutK(g.id), "")] : []
       | Secondary(s) =>
         switch (s.content) {
         | Comment(c) when List.mem(s.id, ids) => [(CommentK(s.id), c)]
         | _ => []
         }
       | Projector(_) => []
       }
     );

/* keyed tokens of a window with their Code.view classes — the merge
   ghosts are REAL-styled spans (same recipe as the drag ghosts) */
let rec keyed_cls_tokens_of_segment =
        (ids: list(Id.t), seg: Segment.t): list((key, string, string)) =>
  seg
  |> List.concat_map((piece: Piece.t) =>
       switch (piece) {
       | Tile(t) =>
         let plurality = List.length(t.label) == 1 ? "mono" : "poly";
         let sort_cls = Sort.class_of(t.mold.out);
         (
           List.mem(t.id, ids)
             ? t.shards
               |> List.filter_map(i =>
                    switch (List.nth_opt(t.label, i)) {
                    | Some(txt) =>
                      let cls =
                        ["token", sort_cls, plurality]
                        @ (Token.is_keyword(txt) ? ["keyword"] : [])
                        |> String.concat(" ");
                      Some((Shard(t.id, i), txt, cls));
                    | None => None
                    }
                  )
             : []
         )
         @ List.concat_map(keyed_cls_tokens_of_segment(ids), t.children);
       | Grout(_)
       | Secondary(_)
       | Projector(_) => []
       }
     );

let animate_enter = (~from: option(float)=?, node: Js.t(Dom.node)): unit => {
  let run = keyframes => {
    let options =
      Animation.Js.options_unsafe({
        duration: dur(enter_duration),
        /* NOT easeOutExpo: it front-loads so hard the entrance reads
           as a pop (91% visible at t=120ms); linear lets the grow
           register (andrew kept missing it) */
        easing: "linear",
      });
    switch (
      Js.Unsafe.meth_call(
        node,
        "animate",
        [|
          Js.Unsafe.inject(Animation.Js.keyframes_unsafe(keyframes)),
          Js.Unsafe.inject(options),
        |],
      )
    ) {
    | exception _ => ()
    | anim => active := [anim, ...active^]
    };
  };
  switch (from) {
  | Some(t0) =>
    /* the ghost carried the entrance to (opacity t0, scale
       0.1+0.9*t0) — continue both, don't restart */
    run([("opacity", Printf.sprintf("%f", t0)), ("opacity", "1")]);
    run([
      ("transform", Printf.sprintf("scale(%f)", 0.1 +. 0.9 *. t0)),
      ("transform", "scale(1)"),
    ]);
  | None =>
    run([("opacity", "0"), ("opacity", "1")]);
    run([("transform", "scale(0.1)"), ("transform", "scale(1)")]);
  };
};

/* emergeFrom flight (D2 emergeMode=clone): a spawned copy departs its
 * source full-size at full opacity — position is the ONLY animated
 * channel (scaling is the vocabulary of appearing; a clone already
 * exists at the source, it splits off). ~from: the drag ghost carried
 * the flight to t — continue the remaining fraction. Movement
 * duration/easing: flights are flights. */
let animate_emerge =
    (
      ~font_metrics: FontMetrics.t,
      ~from: option(float)=?,
      ~bump: float=0.,
      node: Js.t(Dom.node),
      o: Point.t,
      n: Point.t,
    )
    : unit => {
  let remaining = 1. -. Option.value(from, ~default=0.);
  let dx = float_of_int(o.col - n.col) *. font_metrics.col_width *. remaining;
  let dy =
    (float_of_int(o.row - n.row) *. font_metrics.row_height +. bump)
    *. remaining;
  let keyframes =
    Animation.Js.keyframes_unsafe([
      ("transform", Printf.sprintf("translate(%fpx, %fpx)", dx, dy)),
      ("transform", "translate(0px, 0px)"),
    ]);
  let options =
    Animation.Js.options_unsafe({
      duration: dur(duration),
      easing,
    });
  switch (
    Js.Unsafe.meth_call(
      node,
      "animate",
      [|Js.Unsafe.inject(keyframes), Js.Unsafe.inject(options)|],
    )
  ) {
  | exception _ => ()
  | anim =>
    watch_anim("emerge", node, anim);
    active := [anim, ...active^];
  };
};

let animate_node =
    (
      ~font_metrics: FontMetrics.t,
      ~extra: (float, float)=(0., 0.),
      node: Js.t(Dom.node),
      o: Point.t,
      n: Point.t,
    )
    : unit => {
  /* extra: a px offset already applied to the element visually (a
     drag scrub) — the flight continues from there, never restarts */
  let (ex, ey) = extra;
  let dx = float_of_int(o.col - n.col) *. font_metrics.col_width +. ex;
  let dy = float_of_int(o.row - n.row) *. font_metrics.row_height +. ey;
  let keyframes =
    Animation.Js.keyframes_unsafe([
      ("transform", Printf.sprintf("translate(%fpx, %fpx)", dx, dy)),
      ("transform", "translate(0px, 0px)"),
    ]);
  let options =
    Animation.Js.options_unsafe({
      duration: dur(duration),
      easing,
    });
  switch (
    Js.Unsafe.meth_call(
      node,
      "animate",
      [|Js.Unsafe.inject(keyframes), Js.Unsafe.inject(options)|],
    )
  ) {
  | exception _ => () /* no WAAPI: just show the final state */
  | anim =>
    watch_anim("flight", node, anim);
    active := [anim, ...active^];
  };
};

/* Transforms are silently ignored on non-replaced inline boxes: such
 * an element "animates" without rendering any motion (this bit us
 * twice: vdom clobbering an inline display style, then comment spans
 * missing the stylesheet rule). Warn so the next uncovered element
 * kind is loud instead of a mystery snap. SVG is replaced content —
 * transformable even when display:inline. */
/* warn once per class: a batch may mix covered and uncovered kinds */
let warned: ref(list(string)) = ref([]);

/* one getComputedStyle per element CLASS ever — probing every node
   forces a style recalc against the freshly-patched tree (36ms of a
   600ms edit frame, trace-attributed); the guard's job is to catch
   NEW element kinds, so vet by class first, probe once */
let vetted: ref(list(string)) = ref([]);

let warn_invisible = (node: Js.t(Dom.node)): unit =>
  switch (
    {
      let cls = Js.to_string(Js.Unsafe.get(node, "className"));
      if (List.mem(cls, vetted^)) {
        None;
      } else {
        vetted := [cls, ...vetted^];
        let name =
          Js.to_string(Js.Unsafe.get(node, "nodeName"))
          |> String.lowercase_ascii;
        let display =
          Js.Unsafe.meth_call(
            Js.Unsafe.global##.window,
            "getComputedStyle",
            [|Js.Unsafe.inject(node)|],
          )
          |> (cs => Js.to_string(Js.Unsafe.get(cs, "display")));
        name != "svg" && display == "inline" ? Some(cls) : None;
      };
    }
  ) {
  | exception _ => ()
  | None => ()
  | Some(cls) when List.mem(cls, warned^) => ()
  | Some(cls) =>
    warned := [cls, ...warned^];
    print_endline(
      "CodeFlip: element '"
      ++ cls
      ++ "' is display:inline; its movement animation will not render"
      ++ " (add it to the .code inline-block rule in editor.css)",
    );
  };

/* (pre-edit measured, enters allowed): grow-ins are reserved for
   refactorings — under animate-all-edits every typed character and
   TyDi completion re-animating its entrance is nauseating (andrew).
   Movement FLIP stays for all edits. */
let pending: ref(option((Measured.t, Segment.t, bool))) = ref(None);

/* shake the tokens of specific pieces (targeted refusal feedback:
   e.g. the uses that would unbind if a lift fired) */
let shake_nodes = (nodes: list(Js.t(Dom.node))): unit => {
  let keyframes =
    Animation.Js.keyframes_unsafe([
      ("transform", "translateX(0px)"),
      ("transform", "translateX(-3px)"),
      ("transform", "translateX(3px)"),
      ("transform", "translateX(-2px)"),
      ("transform", "translateX(0px)"),
    ]);
  /* blockers read RED — a hard refusal must not look like the
     insist prompt's press-again shake (one meaning per signal) */
  let tint =
    Animation.Js.keyframes_unsafe([
      ("color", "#d43b3b"),
      ("color", "#d43b3b"),
    ]);
  let options =
    Animation.Js.options_unsafe({
      duration: 260,
      easing: "ease-out",
    });
  nodes
  |> List.iter(node =>
       [keyframes, tint]
       |> List.iter(kf =>
            switch (
              Js.Unsafe.meth_call(
                node,
                "animate",
                [|Js.Unsafe.inject(kf), Js.Unsafe.inject(options)|],
              )
            ) {
            | exception _ => ()
            | _ => ()
            }
          )
     );
};

let shake_tokens = (~syntax: CachedSyntax.t, ids: list(Id.t)): unit =>
  switch (JsUtil.get_elem_by_id_opt("caret")) {
  | None => ()
  | Some(caret) =>
    Js.Opt.iter(caret##.parentNode, deco =>
      Js.Opt.iter(
        deco##.parentNode,
        container => {
          let ct =
            Js.Unsafe.meth_call(
              container,
              "querySelector",
              [|
                Js.Unsafe.inject(Js.string(":scope > .code > .code-text")),
              |],
            );
          Js.Opt.iter(
            ct,
            ct => {
              let nodes = Dom.list_of_nodeList(ct##.childNodes);
              switch (pair(entries_of_segment(syntax.segment), nodes)) {
              | None => ()
              | Some(pairs) =>
                pairs
                |> List.filter_map(((k, node)) =>
                     switch (k) {
                     | Shard(id, _) when List.mem(id, ids) => Some(node)
                     | _ => None
                     }
                   )
                |> shake_nodes
              };
            },
          );
        },
      )
    )
  };

/* blocker shakes must fire AFTER the render: the press's render
   replaces token nodes and a WAAPI animation dies with its element
   (the flip silent-kill, measured) — so the update stages ids here
   and go() consumes them post-display, exactly like flights */
let shake_pending: ref(list(Id.t)) = ref([]);
let request_shake = (ids: list(Id.t)): unit => shake_pending := ids;

/* brief horizontal shake on the caret + indication backing: the
   insist prompt (a remedied move is available; press again) */
let shake_insist = (): unit => {
  let ids = ["caret"] @ JsUtil.ids_with_prefix("indication-");
  let keyframes =
    Animation.Js.keyframes_unsafe([
      ("transform", "translateX(0px)"),
      ("transform", "translateX(-3px)"),
      ("transform", "translateX(3px)"),
      ("transform", "translateX(-2px)"),
      ("transform", "translateX(0px)"),
    ]);
  let options =
    Animation.Js.options_unsafe({
      duration: 220,
      easing: "ease-out",
    });
  ids
  |> List.iter(id =>
       switch (JsUtil.get_elem_by_id_opt(id)) {
       | None => ()
       | Some(el) =>
         switch (
           Js.Unsafe.meth_call(
             el,
             "animate",
             [|Js.Unsafe.inject(keyframes), Js.Unsafe.inject(options)|],
           )
         ) {
         | exception _ => ()
         | _ => ()
         }
       }
     );
};

/* Call during the MVU update, before the edit applies */
let request = (~enters: bool=true, syntax: CachedSyntax.t): unit =>
  pending := Some((syntax.measured, syntax.segment, enters));

/* Drag handoff (CodeDrag): visual px offsets tokens already carry
   from the scrub, keyed like the measured diff; consumed by the next
   go() so each flight starts from the scrubbed position. */
let drag_offsets: ref(list((key, (float, float)))) = ref([]);
let set_drag_offsets = (l: list((key, (float, float)))): unit =>
  drag_offsets := l;

/* Adopt foreign animations (the drag's scrub transforms) into the
   active set: the next go() cancels them at the exact moment the
   commit's own flights take over the same elements. */
let adopt = (anims: list(Js.Unsafe.any)): unit => active := anims @ active^;

/* Anchored decorations: elements whose DOM id is "<prefix><uuid>"
   ride the token with that id — one rail for the drag scrub and the
   commit flights (a deco missing an anchor just doesn't move). */
let deco_prefixes = [
  "varhl-",
  "errdec-",
  "warndec-",
  "indication-",
  "projdec-",
];

/* anchors are PER-SHARD where the id encodes one (indication-
   <uuid>-<k>): delimiters of one tile don't move rigidly (case vs
   end), so a tile-level delta smears — each deco rides exactly its
   own token */
let anchored_decos = (): list((Id.t, option(int), Js.t(Dom.node))) =>
  deco_prefixes
  |> List.concat_map(prefix =>
       JsUtil.ids_with_prefix(prefix)
       |> List.filter_map(dom_id => {
            let rest =
              String.sub(
                dom_id,
                String.length(prefix),
                String.length(dom_id) - String.length(prefix),
              );
            /* a uuid is 36 chars; an optional -<shard> follows */
            let uuid =
              String.length(rest) > 36 ? String.sub(rest, 0, 36) : rest;
            let shard =
              if (String.length(rest) > 37) {
                switch (
                  int_of_string_opt(
                    String.sub(rest, 37, String.length(rest) - 37),
                  )
                ) {
                | s => s
                | exception _ => None
                };
              } else {
                None;
              };
            switch (Id.of_string(uuid)) {
            | exception _ => None
            | None => None
            | Some(id) =>
              JsUtil.get_elem_by_id_opt(dom_id)
              |> Option.map(el => (id, shard, (el :> Js.t(Dom.node))))
            };
          })
     );

/* a deco anchor's measurement: its exact shard when known */
let anchor_meas =
    (m: Measured.t, id: Id.t, shard: option(int))
    : option(Measured.measurement) =>
  switch (shard) {
  | Some(k) =>
    switch (find_meas(m, Shard(id, k))) {
    | Some(meas) => Some(meas)
    | None => Measured.find_by_id(id, m)
    }
  | None => Measured.find_by_id(id, m)
  };

/* Dead-press feedback: shake the indicated construct's backing
   shards (they exist and carry DOM ids whenever something is
   indicated) so a gated gesture visibly registers instead of
   silently doing nothing. Falls back to the caret. */
let shake_dead_press =
    (
      ~segment: option(Segment.t)=?,
      ~axis: [
         | `X
         | `Y
       ]=`X,
      (),
    )
    : unit => {
  let ids = JsUtil.ids_with_prefix("indication-");
  /* the indicated construct's TEXT shakes with its backing: pair the
     segment against .code-text and take the tokens whose tile is an
     indication anchor (backing-only reads as nothing — andrew) */
  let indicated_uuids =
    ids
    |> List.filter_map(dom_id =>
         String.length(dom_id) > 47
           ? Some(String.sub(dom_id, 11, 36)) : None
       );
  let text_nodes =
    switch (segment) {
    | Some(segment) when indicated_uuids != [] =>
      switch (JsUtil.get_elem_by_id_opt("caret")) {
      | None => []
      | Some(caret) =>
        switch (
          Js.Opt.to_option(caret##.parentNode)
          |> Option.map(deco => Js.Opt.to_option(deco##.parentNode))
          |> Option.join
        ) {
        | None => []
        | Some(container) =>
          let ct =
            Js.Unsafe.meth_call(
              container,
              "querySelector",
              [|
                Js.Unsafe.inject(Js.string(":scope > .code > .code-text")),
              |],
            );
          switch (Js.Opt.to_option(ct)) {
          | None => []
          | Some(ct) =>
            let nodes = Dom.list_of_nodeList(ct##.childNodes);
            switch (pair(entries_of_segment(segment), nodes)) {
            | None => []
            | Some(pairs) =>
              pairs
              |> List.filter_map(((k, node)) =>
                   switch (k) {
                   | Shard(id, _)
                       when List.mem(Id.to_string(id), indicated_uuids) =>
                     Some(node)
                   | _ => None
                   }
                 )
            };
          };
        }
      }
    | _ => []
    };
  let backing =
    switch (ids) {
    | [] =>
      switch (JsUtil.get_elem_by_id_opt("caret")) {
      | Some(el) => [(el :> Js.t(Dom.node))]
      | None => []
      }
    | ids =>
      ids
      |> List.filter_map(JsUtil.get_elem_by_id_opt)
      |> List.map(el => (el :> Js.t(Dom.node)))
    };
  /* wiggle along the attempted direction: a refused vertical move
     shakes vertically */
  let tr = (px: int) =>
    switch (axis) {
    | `X => Printf.sprintf("translateX(%dpx)", px)
    | `Y => Printf.sprintf("translateY(%dpx)", px)
    };
  backing
  @ text_nodes
  |> List.iter(node => {
       let keyframes =
         Animation.Js.keyframes_unsafe([
           ("transform", tr(0)),
           ("transform", tr(-4)),
           ("transform", tr(4)),
           ("transform", tr(-3)),
           ("transform", tr(2)),
           ("transform", tr(0)),
         ]);
       let options =
         Animation.Js.options_unsafe({
           duration: 280,
           easing: "ease-in-out",
         });
       switch (
         Js.Unsafe.meth_call(
           node,
           "animate",
           [|Js.Unsafe.inject(keyframes), Js.Unsafe.inject(options)|],
         )
       ) {
       | exception _ => ()
       | _ => ()
       };
     });
};

/* Commit-time scroll bump (pinned-frame extract): applied when the
   flights start — same frame as the layout change, after the patch —
   so nothing visibly moves; every flight offset gets the bump added
   since document coordinates shifted under the pinned content. */
let scroll_bump: ref(option((Js.t(Dom_html.element), int))) = ref(None);

let set_scroll_bump = (~rows: int, ~near: Js.t(Dom_html.element)): unit => {
  /* nearest scrollable ancestor */
  let rec scroller = (el: Js.t(Dom_html.element)) =>
    if (el##.scrollHeight > el##.clientHeight) {
      Some(el);
    } else {
      switch (Js.Opt.to_option(el##.parentNode)) {
      | Some(p) =>
        switch (Js.Opt.to_option(Dom_html.CoerceTo.element(p))) {
        | Some(p) => scroller(p)
        | None => None
        }
      | None => None
      };
    };
  switch (scroller(near)) {
  | Some(el) => scroll_bump := Some((el, rows))
  | None => scroll_bump := None
  };
};

/* Call after render. The active editor's .code-text is located via
 * the caret: caret lives in .code-container > .code-deco, a sibling
 * of .code > .code-text (the scoped selector avoids matching code
 * rendered inside probe projections further down the container). */
let go = (~syntax: CachedSyntax.t, ~font_metrics: FontMetrics.t): unit => {
  switch (shake_pending^) {
  | [] => ()
  | ids =>
    shake_pending := [];
    shake_tokens(~syntax, ids);
  };
  switch (pending^) {
  | None => flip_log([("k", str("render"))])
  | Some((old_m, old_seg, enters_ok)) =>
    flip_log([
      ("k", str("batch")),
      ("cancelled_prior", int_(List.length(active^))),
    ]);
    pending := None;
    /* stale animations — including adopted drag scrubs (fill:both,
       they'd re-assert after any new flight ends) — must not outlive
       the render they were staged against */
    cancel_active();
    let offsets = drag_offsets^;
    drag_offsets := [];
    let enter_from = drag_enter_from^;
    drag_enter_from := None;
    let emerge = emerge_src^;
    emerge_src := [];
    let (merge_dissolved, merge_survivor) = merge_staged^;
    merge_staged := ([], []);
    let bump_y =
      switch (scroll_bump^) {
      | Some((el, rows)) =>
        let px = float_of_int(rows) *. font_metrics.row_height;
        el##.scrollTop :=  el##.scrollTop + int_of_float(px);
        px;
      | None => 0.
      };
    scroll_bump := None;
    let new_m = syntax.measured;
    switch (JsUtil.get_elem_by_id_opt("caret")) {
    | None => ()
    | Some(caret) =>
      Js.Opt.iter(caret##.parentNode, deco =>
        Js.Opt.iter(
          deco##.parentNode,
          container => {
            let ct =
              Js.Unsafe.meth_call(
                container,
                "querySelector",
                [|
                  Js.Unsafe.inject(Js.string(":scope > .code > .code-text")),
                |],
              );
            Js.Opt.iter(
              ct,
              ct => {
                let nodes = Dom.list_of_nodeList(ct##.childNodes);
                switch (pair(entries_of_segment(syntax.segment), nodes)) {
                | None =>
                  flip_log([("k", str("bail")), ("why", str("pair"))])
                | Some(pairs) =>
                  /* pair() builds by prepending — restore traversal
                     order (the emerge zip is positional) */
                  let pairs = List.rev(pairs);
                  /* mergeInto (emerge reversed): ghost copies of the
                     dissolved window depart their old positions and
                     converge onto the surviving window, full opacity
                     (D2 clone), removed on arrival — the identical
                     text beneath makes the removal invisible. */
                  if (merge_dissolved != [] && merge_survivor != []) {
                    let olds =
                      keyed_cls_tokens_of_segment(merge_dissolved, old_seg)
                      |> List.filter_map(((k, txt, cls)) =>
                           find_meas(old_m, k)
                           |> Option.map((m: Measured.measurement) =>
                                (txt, cls, m.origin)
                              )
                         );
                    let news =
                      keyed_cls_tokens_of_segment(
                        merge_survivor,
                        syntax.segment,
                      )
                      |> List.filter_map(((k, _, _)) =>
                           find_meas(new_m, k)
                           |> Option.map((m: Measured.measurement) =>
                                m.origin
                              )
                         );
                    if (List.length(olds) == List.length(news) && olds != []) {
                      let rect =
                        Js.Unsafe.meth_call(
                          ct,
                          "getBoundingClientRect",
                          [||],
                        );
                      let base_x: float = Js.Unsafe.get(rect, "left");
                      let base_y: float = Js.Unsafe.get(rect, "top");
                      remove_merge_overlay();
                      let overlay = Dom_html.createDiv(Dom_html.document);
                      overlay##.id := Js.string(merge_overlay_id);
                      overlay##.className := Js.string("code");
                      overlay##.style##.cssText :=
                        Js.string(
                          "position:fixed;inset:0;pointer-events:none;z-index:999999;",
                        );
                      Dom.appendChild(Dom_html.document##.body, overlay);
                      let px = (pt: Measured.Point.t) => (
                        base_x
                        +. float_of_int(pt.col)
                        *. font_metrics.col_width,
                        base_y
                        +. float_of_int(pt.row)
                        *. font_metrics.row_height,
                      );
                      let first = ref(true);
                      List.combine(olds, news)
                      |> List.iter((((txt, cls, o), n)) => {
                           let (ox, oy) = px(o);
                           let (nx, ny) = px(n);
                           let span = Dom_html.createSpan(Dom_html.document);
                           span##.className := Js.string(cls);
                           span##.textContent := Js.some(Js.string(txt));
                           span##.style##.cssText :=
                             Js.string(
                               Printf.sprintf(
                                 "position:absolute;left:%fpx;top:%fpx;",
                                 nx,
                                 ny,
                               ),
                             );
                           Dom.appendChild(overlay, span);
                           let keyframes =
                             Animation.Js.keyframes_unsafe([
                               (
                                 "transform",
                                 Printf.sprintf(
                                   "translate(%fpx, %fpx)",
                                   ox -. nx,
                                   oy -. ny,
                                 ),
                               ),
                               ("transform", "translate(0px, 0px)"),
                             ]);
                           let options =
                             Animation.Js.options_unsafe({
                               duration: dur(duration),
                               easing,
                             });
                           switch (
                             Js.Unsafe.meth_call(
                               span,
                               "animate",
                               [|
                                 Js.Unsafe.inject(keyframes),
                                 Js.Unsafe.inject(options),
                               |],
                             )
                           ) {
                           | exception _ => ()
                           | anim =>
                             active := [anim, ...active^];
                             if (first^) {
                               first := false;
                               Js.Unsafe.set(
                                 anim,
                                 "onfinish",
                                 Js.wrap_callback(_ => remove_merge_overlay()),
                               );
                             };
                           };
                         });
                    } else if (olds != []) {
                      Printf.eprintf(
                        "CodeFlip: merge staged but windows differ (%d old vs %d new tokens) — no convergence\n",
                        List.length(olds),
                        List.length(news),
                      );
                    };
                  };
                  let moved =
                    pairs
                    |> List.filter_map(((k, node)) =>
                         switch (
                           find_meas_end_aligned(
                             ~m_seg=old_seg,
                             ~other_seg=syntax.segment,
                             ~other=new_m,
                             old_m,
                             k,
                           ),
                           find_meas(new_m, k),
                         ) {
                         | (Some(o), Some(n))
                             when
                               o.origin != n.origin
                               && o.origin.row == o.last.row
                               && n.origin.row == n.last.row =>
                           Some((k, node, o.origin, n.origin))
                         | _ => None
                         }
                       );
                  let entered =
                    !enters_ok
                      ? []
                      : pairs
                        |> List.filter_map(((k, node)) =>
                             switch (
                               find_meas(old_m, k),
                               find_meas(new_m, k),
                             ) {
                             | (None, Some(_)) => Some((k, node))
                             | _ => None
                             }
                           );
                  flip_log([
                    ("k", str("staged")),
                    ("moved", int_(List.length(moved))),
                    ("entered", int_(List.length(entered))),
                    (
                      "capped",
                      Js.Unsafe.inject(
                        Js.bool(
                          List.length(moved)
                          + List.length(entered) > max_moved,
                        ),
                      ),
                    ),
                  ]);
                  if ((moved != [] || entered != [])
                      && List.length(moved)
                      + List.length(entered) <= max_moved) {
                    moved
                    |> List.iter(((k, node, o, n)) => {
                         let (ex, ey) =
                           List.assoc_opt(k, offsets)
                           |> Option.value(~default=(0., 0.));
                         animate_node(
                           ~font_metrics,
                           ~extra=(ex, ey +. bump_y),
                           node,
                           o,
                           n,
                         );
                       });
                    /* anchored decorations fly with their tokens;
                       elements re-found post-render (vdom may have
                       replaced them), offsets looked up by the
                       anchor's shard keys */
                    anchored_decos()
                    |> List.iter(((id, shard, node)) =>
                         switch (
                           anchor_meas(old_m, id, shard),
                           anchor_meas(new_m, id, shard),
                         ) {
                         | (Some(o), Some(n)) when o.origin != n.origin =>
                           let (ex, ey) =
                             offsets
                             |> List.find_opt(((k, _)) =>
                                  switch (k) {
                                  | Shard(kid, _)
                                  | GroutK(kid)
                                  | CommentK(kid) => kid == id
                                  }
                                )
                             |> Option.map(snd)
                             |> Option.value(~default=(0., 0.));
                           animate_node(
                             ~font_metrics,
                             ~extra=(ex, ey +. bump_y),
                             node,
                             o.origin,
                             n.origin,
                           );
                         | _ => ()
                         }
                       );
                    /* emergeFrom: the clone is a CONTIGUOUS run of
                       entered keys whose (kind, text) sequence
                       matches the source's tokens in the OLD
                       segment. Entered ⊃ clone is normal on stale
                       buffers: rewrite_node's dedupe_ids heals
                       duplicate ids program-wide at commit, so
                       unrelated re-minted subtrees enter alongside
                       the clone — they grow in place while the
                       clone still flies. Exactly one matching
                       window flies; zero or several (ambiguous) →
                       all grow (never guess). */
                    let srcs =
                      emerge == []
                        ? [] : keyed_tokens_of_segment(emerge, old_seg);
                    let node_text = (node: Js.t(Dom.node)): string =>
                      switch (
                        Js.Opt.to_option(Js.Unsafe.get(node, "textContent"))
                      ) {
                      | Some(s) => Js.to_string(s)
                      | None => ""
                      | exception _ => ""
                      };
                    let ents = Array.of_list(entered);
                    let n_src = List.length(srcs);
                    let windows =
                      n_src == 0 || Array.length(ents) < n_src
                        ? []
                        : List.init(Array.length(ents) - n_src + 1, i => i)
                          |> List.filter(i =>
                               srcs
                               |> List.mapi((j, s) => (j, s))
                               |> List.for_all(((j, (sk, stext))) => {
                                    let (ek, enode) = ents[i + j];
                                    key_kind_match(sk, ek)
                                    && (
                                      stext == "" || stext == node_text(enode)
                                    );
                                  })
                             );
                    /* FAN-OUT: when the matching windows are
                       mutually disjoint, they are all certain — an
                       inline with N surviving uses spawns N copies
                       at the def and every one FLIES to its use
                       (the fruit bowl, finally whole). Overlapping
                       windows are genuinely ambiguous -> grow-in. */
                    let disjoint =
                      switch (windows) {
                      | [] => false
                      | ws =>
                        let sorted = List.sort(compare, ws);
                        let rec ok = w =>
                          switch (w) {
                          | []
                          | [_] => true
                          | [a, b, ...rest] =>
                            b >= a + n_src && ok([b, ...rest])
                          };
                        ok(sorted);
                      };
                    if (disjoint) {
                      let in_window = i =>
                        windows |> List.exists(w => i >= w && i < w + n_src);
                      windows
                      |> List.iter(w =>
                           List.iteri(
                             (j, (sk, _)) => {
                               let (k, node) = ents[w + j];
                               switch (
                                 find_meas(old_m, sk),
                                 find_meas(new_m, k),
                               ) {
                               | (Some(o), Some(n))
                                   when
                                     o.origin.row == o.last.row
                                     && n.origin.row == n.last.row =>
                                 animate_emerge(
                                   ~font_metrics,
                                   ~from=?enter_from,
                                   ~bump=bump_y,
                                   node,
                                   o.origin,
                                   n.origin,
                                 )
                               | _ => animate_enter(node)
                               };
                             },
                             srcs,
                           )
                         );
                      ents
                      |> Array.iteri((i, (_, node)) =>
                           in_window(i) ? () : animate_enter(node)
                         );
                    } else {
                      if (emerge != [] && entered != []) {
                        print_endline(
                          Printf.sprintf(
                            "CodeFlip: emerge staged (%d source tokens) but %s clone window among %d entered — grow-in fallback",
                            n_src,
                            windows == [] ? "no" : "no disjoint",
                            List.length(entered),
                          ),
                        );
                      };
                      entered
                      |> List.iter(((_, node)) =>
                           animate_enter(~from=?enter_from, node)
                         );
                    };
                    moved
                    |> List.iter(((_, node, _, _)) => warn_invisible(node));
                  };
                };
              },
            );
          },
        )
      )
    };
  };
};
