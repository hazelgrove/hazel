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

/* Slightly statelier than the caret's 125ms (code moves farther);
 * for slow-motion debugging use ~560 / "ease-in-out" */
let duration = 180;
let easing = Animation.easeOutExpo;

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

/* Pair entries with .code-text's childNodes; None on any mismatch
 * (bail out, skip animation — never guess at correlation) */
let pair =
    (entries: list(entry), nodes: list(Js.t(Dom.node)))
    : option(list((key, Js.t(Dom.node)))) => {
  let rec walk = (entries, nodes, acc) =>
    switch (entries, nodes) {
    | ([], []) => Some(acc)
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

let cancel_active = (): unit => {
  active^
  |> List.iter(anim =>
       switch (Js.Unsafe.meth_call(anim, "cancel", [||])) {
       | exception _ => ()
       | _ => ()
       }
     );
  active := [];
};

/* EXPERIMENT (andrew): newly created elements fade in (they used to
 * pop). Exit animation is structurally out of reach here: removed
 * elements are already gone from the DOM when go() runs. */
let enter_duration = 320; /* slower than movement so it registers */

let animate_enter = (node: Js.t(Dom.node)): unit => {
  let run = keyframes => {
    let options =
      Animation.Js.options_unsafe({
        duration: enter_duration,
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
  run([("opacity", "0"), ("opacity", "1")]);
  run([("transform", "scale(0.1)"), ("transform", "scale(1)")]);
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
      duration,
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
  | anim => active := [anim, ...active^]
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

let warn_invisible = (node: Js.t(Dom.node)): unit =>
  switch (
    {
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
      name != "svg" && display == "inline"
        ? Some(Js.to_string(Js.Unsafe.get(node, "className"))) : None;
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

let pending: ref(option(Measured.t)) = ref(None);

/* Call during the MVU update, before the edit applies */
let request = (syntax: CachedSyntax.t): unit =>
  pending := Some(syntax.measured);

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
let deco_prefixes = ["varhl-", "errdec-", "warndec-", "indication-"];

let anchored_decos = (): list((Id.t, Js.t(Dom.node))) =>
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
            /* indication ids carry a -<shard> suffix; a uuid is
               36 chars */
            let uuid =
              String.length(rest) > 36 ? String.sub(rest, 0, 36) : rest;
            switch (Id.of_string(uuid)) {
            | exception _ => None
            | None => None
            | Some(id) =>
              JsUtil.get_elem_by_id_opt(dom_id)
              |> Option.map(el => (id, (el :> Js.t(Dom.node))))
            };
          })
     );

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
let go = (~syntax: CachedSyntax.t, ~font_metrics: FontMetrics.t): unit =>
  switch (pending^) {
  | None => ()
  | Some(old_m) =>
    pending := None;
    /* stale animations — including adopted drag scrubs (fill:both,
       they'd re-assert after any new flight ends) — must not outlive
       the render they were staged against */
    cancel_active();
    let offsets = drag_offsets^;
    drag_offsets := [];
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
                | None => ()
                | Some(pairs) =>
                  let moved =
                    pairs
                    |> List.filter_map(((k, node)) =>
                         switch (find_meas(old_m, k), find_meas(new_m, k)) {
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
                    pairs
                    |> List.filter_map(((k, node)) =>
                         switch (find_meas(old_m, k), find_meas(new_m, k)) {
                         | (None, Some(_)) => Some(node)
                         | _ => None
                         }
                       );
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
                    |> List.iter(((id, node)) =>
                         switch (
                           Measured.find_by_id(id, old_m),
                           Measured.find_by_id(id, new_m),
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
                    entered |> List.iter(animate_enter);
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
