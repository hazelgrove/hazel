/* A tool's accepted syntax change compiled into definition-sized splices.
   Parsing, validation and tool-result reporting stay atomic. These operations
   retain the accepted edit's piece ids and can be applied independently to
   the presentation program. Nested blocks grow inside their parent shell. */
open DefinitionSpans;
type path = list((Id.t, int));
type operation = {
  path,
  at: int,
  remove: int,
  insert: Segment.t,
  target: option(Id.t),
};

let rec map_at = (path: path, f, seg: Segment.t): Segment.t =>
  switch (path) {
  | [] => f(seg)
  | [(id, child), ...rest] =>
    List.map(
      p =>
        switch (p) {
        | Piece.Tile(t) when t.id == id =>
          Piece.Tile({
            ...t,
            children:
              List.mapi(
                (i, s) => i == child ? map_at(rest, f, s) : s,
                t.children,
              ),
          })
        | _ => p
        },
      seg,
    )
  };

let apply = (op: operation, seg: Segment.t): Segment.t =>
  map_at(
    op.path,
    s => take(op.at, s) @ op.insert @ drop(op.at + op.remove, s),
    seg,
  );

type item = {
  id: option(Id.t),
  pieces: Segment.t,
};
let items = seg => {
  let start = ref(0);
  let spans = item_spans(seg);
  let result =
    List.map(
      sp => {
        let pieces = slice(start^, sp.sp_stop, seg);
        start := sp.sp_stop;
        {
          id: sp.sp_id,
          pieces,
        };
      },
      spans,
    );
  result == [] && seg != []
    ? [
      {
        id: None,
        pieces: seg,
      },
    ]
    : result;
};
let has_defs = seg =>
  List.exists(
    sp => sp.sp_kind == IDef,
    item_spans(~divided_only_tail=true, seg),
  );
let tail = seg =>
  item_spans(seg)
  |> List.filter(sp => sp.sp_kind != IDef)
  |> List.concat_map(sp => slice(sp.sp_start, sp.sp_stop, seg));

/* Use old child blocks in a changed/new parent shell, then update those
   blocks in their own operations. Ordinary expressions remain one edit. */
let rec shell = (path, before: Segment.t, after: Segment.t) => {
  let blocks = ref([]);
  let result =
    List.map(
      p =>
        switch (p) {
        | Piece.Tile(t) =>
          let old =
            List.find_map(
              p =>
                switch (p) {
                | Piece.Tile(b) when b.id == t.id => Some(b)
                | _ => None
                },
              before,
            );
          Piece.Tile({
            ...t,
            children:
              List.mapi(
                (i, child) => {
                  let previous =
                    Option.bind(old, b => List.nth_opt(b.children, i))
                    |> Option.value(~default=[]);
                  let child_path = path @ [(t.id, i)];
                  if (has_defs(child) || has_defs(previous)) {
                    let start = previous == [] ? tail(child) : previous;
                    blocks := blocks^ @ [(child_path, start, child)];
                    start;
                  } else {
                    let (s, bs) = shell(child_path, previous, child);
                    blocks := blocks^ @ bs;
                    s;
                  };
                },
                t.children,
              ),
          });
        | _ => p
        },
      after,
    );
  (result, blocks^);
};

let plan = (before: Segment.t, after: Segment.t): list(operation) => {
  let ops = ref([]);
  let emit = op => ops := [op, ...ops^];
  let rec block = (path, before, after) => {
    let current = ref(items(before));
    let offset = i =>
      take(i, current^)
      |> List.fold_left((n, it) => n + List.length(it.pieces), 0);
    List.iteri(
      (i, next: item) => {
        /* A moved item is removed before being inserted at its new site;
           it must never be present twice with the same syntax ids. */
        let index =
          drop(i, current^)
          |> List.mapi((j, it: item) => (i + j, it))
          |> List.find_map(((j, it)) => it.id == next.id ? Some(j) : None);
        switch (index) {
        | Some(j) when j != i =>
          let old = List.nth(current^, j);
          emit({
            path,
            at: offset(j),
            remove: List.length(old.pieces),
            insert: [],
            target: old.id,
          });
          current := take(j, current^) @ drop(j + 1, current^);
        | _ => ()
        };
        let matching =
          switch (List.nth_opt(current^, i)) {
          | Some(it) when it.id == next.id => Some(it)
          | _ => None
          };
        let old =
          Option.map((it: item) => it.pieces, matching)
          |> Option.value(~default=[]);
        let (initial, children) = shell(path, old, next.pieces);
        if (old != initial) {
          emit({
            path,
            at: offset(i),
            remove: List.length(old),
            insert: initial,
            target: next.id,
          });
        };
        current :=
          take(i, current^)
          @ [
            {
              ...next,
              pieces: initial,
            },
          ]
          @ drop(i + (matching == None ? 0 : 1), current^);
        List.iter(((p, b, a)) => block(p, b, a), children);
        current := List.mapi((j, it) => j == i ? next : it, current^);
      },
      items(after),
    );
    let keep = List.length(items(after));
    while (List.length(current^) > keep) {
      let i = List.length(current^) - 1;
      let old = List.nth(current^, i);
      emit({
        path,
        at: offset(i),
        remove: List.length(old.pieces),
        insert: [],
        target: old.id,
      });
      current := take(i, current^);
    };
  };
  block([], before, after);
  List.rev(ops^);
};

/* Grout is ephemeral structural padding. Inserting/removing a member can
   expose an empty expression or a trailing separator; repair those shapes
   for rendering without changing the raw operation offsets or durable ids. */
let materialize = seg =>
  Segment.regrout((Nib.Shape.concave(), Nib.Shape.concave()), seg);
