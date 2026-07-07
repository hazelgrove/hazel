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

let animate_node =
    (
      ~font_metrics: FontMetrics.t,
      node: Js.t(Dom.node),
      o: Point.t,
      n: Point.t,
    )
    : unit => {
  let dx = float_of_int(o.col - n.col) *. font_metrics.col_width;
  let dy = float_of_int(o.row - n.row) *. font_metrics.row_height;
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

let pending: ref(option(Measured.t)) = ref(None);

/* Call during the MVU update, before the edit applies */
let request = (syntax: CachedSyntax.t): unit =>
  pending := Some(syntax.measured);

/* Call after render. The active editor's .code-text is located via
 * the caret: caret lives in .code-container > .code-deco, a sibling
 * of .code > .code-text (the scoped selector avoids matching code
 * rendered inside probe projections further down the container). */
let go = (~syntax: CachedSyntax.t, ~font_metrics: FontMetrics.t): unit =>
  switch (pending^) {
  | None => ()
  | Some(old_m) =>
    pending := None;
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
                           Some((node, o.origin, n.origin))
                         | _ => None
                         }
                       );
                  if (moved != [] && List.length(moved) <= max_moved) {
                    cancel_active();
                    moved
                    |> List.iter(((node, o, n)) =>
                         animate_node(~font_metrics, node, o, n)
                       );
                  };
                };
              },
            );
          },
        )
      )
    };
  };
