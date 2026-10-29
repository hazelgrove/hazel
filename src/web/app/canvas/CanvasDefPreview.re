open Virtual_dom.Vdom;
open Haz3lcore;

/* Read-only rendering of a definition's body for the info panel in the
   placements where the editor is elsewhere (sidebar, split). Memoized on
   the whole buffer's identity + the definition id: find_def slices a
   fresh list per call, the buffer itself is a persistent value. */
let memo: ref(option((Segment.t, Id.t, Node.t))) =
  ref(None: option((Segment.t, Id.t, Node.t)));

let view =
    (~globals: Globals.t, ~key: Segment.t, ~id: Id.t, def: Segment.t): Node.t =>
  switch (memo^) {
  | Some((k, i, n)) when k === key && i == id => n
  | _ =>
    let n =
      Node.div(
        ~attrs=[Attr.classes(["canvas-def-code", "code-container"])],
        [CodeViewable.view_segment(~globals, def)],
      );
    memo := Some((key, id, n));
    n;
  };
