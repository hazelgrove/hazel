open Haz3lcore;

/* Shared helpers for corpus-driven tests (the mega programs in
   hazel-programs/mega). Paths resolve from either the repo root or
   test/ (run_node.sh runs from the repo root; dune runtest from
   _build). */

let read_file = (path: string): option(string) =>
  switch (open_in_bin(path)) {
  | ic =>
    let n = in_channel_length(ic);
    let s = really_input_string(ic, n);
    close_in(ic);
    Some(s);
  | exception _ => None
  };

let mega_path = (name: string): string => {
  let path = "hazel-programs/mega/" ++ name;
  Sys.file_exists(path) ? path : "../hazel-programs/mega/" ++ name;
};

let mega_src = (name: string): option(string) =>
  read_file(mega_path(name));

let parse = (~root: Sort.t=Exp, src: string): option(Segment.t) =>
  FastParse.of_text(
    ~materialize=Triggers.invoked_projector,
    ~collect_refractors=true,
    ~root,
    src,
  );

let corpus_seg = (~root: Sort.t=Exp, name: string): option(Segment.t) =>
  Option.bind(mega_src(name), parse(~root));

/* in-place single-token rewrite with a fresh id, preserving the
   physical identity of every untouched piece — the shape of a real
   editor edit after the remold identity restore. Returns whether the
   needle was found; replaces EVERY tile whose label is [needle]. */
let rec edit_token =
        (~needle: string, ~repl: string, seg: Segment.t): (Segment.t, bool) => {
  let piece = (p: Piece.t): (Piece.t, bool) =>
    switch (p) {
    | Tile(t) when t.label == [needle] => (
        Tile({
          ...t,
          id: Id.mk(),
          label: [repl],
        }),
        true,
      )
    | Tile(t) =>
      let (children, changed) =
        List.fold_right(
          (seg, (segs, ch)) => {
            let (seg', ch') = edit_token(~needle, ~repl, seg);
            ([seg', ...segs], ch || ch');
          },
          t.children,
          ([], false),
        );
      changed
        ? (
          Tile({
            ...t,
            children,
          }),
          true,
        )
        : (p, false);
    | p => (p, false)
    };
  let (pieces, changed) =
    List.fold_right(
      (p, (ps, ch)) => {
        let (p', ch') = piece(p);
        ([p', ...ps], ch || ch');
      },
      seg,
      ([], false),
    );
  changed ? (pieces, true) : (seg, false);
};

let sorted_ids = (ids: list(Id.t)): list(string) =>
  List.sort_uniq(compare, List.map(Id.to_string, ids));

/* Mega-scale slides/corpora skip super-linear per-slide gates (typing
   parse, roundtrip): minutes each, composed from already-swept
   sources. Cheap gates (load-path text fidelity) still run on them. */
let mega_scale = (name: string): bool =>
  String.length(name) >= 4 && String.sub(name, 0, 4) == "Mega";
