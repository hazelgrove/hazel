/* Duplicate-piece-id watchdog: walks the current buffer's segment
   after each render and console.errors any piece id appearing more
   than once. Duplicate ids are the root of a whole bug class
   (glommed indication, caret jumps, wrong animation pairing) and
   animations failing to play is NOT a reliable canary — flights
   also skip for benign reasons (cancelled by a follow-up render,
   bulk-change cap, pairing bailouts). This is the reliable one.
   Cost: one O(n) hashtbl pass per render; only speaks on breakage.
   Throttled to one report per distinct id set so a persistent dup
   doesn't spam every frame. */

let last_report: ref(string) = ref("");

let rec ids_of_segment =
        (seg: Haz3lcore.Segment.t, acc): list(Haz3lcore.Id.t) =>
  seg
  |> List.fold_left(
       ~f=
         (acc, p: Haz3lcore.Piece.t) =>
           switch (p) {
           | Tile(t) =>
             t.children
             |> List.fold_left(
                  ~f=(acc, c) => ids_of_segment(c, acc),
                  ~init=[t.id, ...acc],
                )
           | Grout(g) => [g.id, ...acc]
           | Secondary(w) => [w.id, ...acc]
           | Projector(pr) => ids_of_segment([pr.syntax], [pr.id, ...acc])
           },
       ~init=acc,
     );

let check = (seg: Haz3lcore.Segment.t): unit => {
  let ids = ids_of_segment(seg, []);
  let seen = Stdlib.Hashtbl.create(List.length(ids));
  let dups =
    ids
    |> List.filter(~f=id =>
         if (Stdlib.Hashtbl.mem(seen, id)) {
           true;
         } else {
           Stdlib.Hashtbl.add(seen, id, ());
           false;
         }
       );
  switch (dups) {
  | [] => last_report := ""
  | _ =>
    let msg =
      dups
      |> List.map(~f=Haz3lcore.Id.to_string)
      |> List.dedup_and_sort(~compare=String.compare)
      |> String.concat(~sep=", ");
    if (!String.equal(msg, last_report^)) {
      last_report := msg;
      Js_of_ocaml.Firebug.console##error(
        Js_of_ocaml.Js.string("DUPLICATE PIECE IDS in buffer: " ++ msg),
      );
    };
  };
};
