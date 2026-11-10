open Util;
open Haz3lcore;
open Language;

module O = Web.OutlineTree;

/* programs loaded, edited through Perform and read back, for the module
   tests: the editor's own editing settings, no evaluation */

/* the editor's own editing settings; no evaluation */
let settings: CoreSettings.t = {
  ...CoreSettings.on,
  dynamics: false,
  elaborate: false,
  assist: false,
};

/* tokens in order, whitespace and grout aside, shards in place */
let rec tokens = (seg: Segment.t): list(string) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Secondary(_)
      | Grout(_)
      | Projector(_) => []
      | Tile(t) =>
        let rec interleave = (ls, chs) =>
          switch (ls, chs) {
          | ([], _) => []
          | ([l, ...ls], []) => [l, ...interleave(ls, [])]
          | ([l, ...ls], [c, ...chs]) =>
            [l, ...tokens(c)] @ interleave(ls, chs)
          };
        interleave(Tile.effective_label(t), t.children);
      },
    seg,
  );

let load = (~root: Sort.t, src: string): Zipper.t =>
  Zipper.unzip(CorpusUtil.seg_of(~root, src));

let key_of = (p: O.path): string =>
  String.concat(
    "/",
    List.map(
      (s: O.path_seg) =>
        (s.s_label == "" ? "⇒" : s.s_label)
        ++ (s.s_occ > 0 ? "#" ++ string_of_int(s.s_occ) : ""),
      p,
    ),
  );

let statics_of = (~root, z: Zipper.t): CachedStatics.t =>
  CachedStatics.init_compositional(
    ~settings,
    ~stitch=Fun.id,
    ~root,
    ~probe_ids=Id.Map.empty,
    z,
  );

/* the text of the edited program, around a point */
let text_of = (z: Zipper.t): string =>
  Printer.of_zipper(~holes="?", ~indent="", z);

let counter_mod = "module Counter = {\n  let start = 0;\n  let next = start + 1;\n  let after = next + 1\n};\nCounter.after";

let read_any = (paths: list(string)): option(string) =>
  List.find_map(CorpusUtil.read_file, paths);

/* the standard library's sources, when this tree has them (stdlib
   branches), else copies in test/programs */
let stdlib = (name: string): option(string) =>
  read_any([
    "hazel-programs/stdlib/" ++ name,
    "../hazel-programs/stdlib/" ++ name,
    "test/programs/" ++ name,
    "../test/programs/" ++ name,
  ]);

let rows_of = (outline: O.t): list((string, Id.t)) =>
  O.flatten(O.rows(outline))
  |> List.filter_map((n: O.node) =>
       switch (n.o_id) {
       | Some(id) when n.o_kind != KTrail && n.o_kind != KTests =>
         Option.map(p => (key_of(p), id), O.label_path(id, outline))
       | _ => None
       }
     );

/* the point of the [k]-th occurrence of [needle], [off] columns in */
let point_of = (~k=0, ~off=0, src: string, needle: string): option(Point.t) => {
  let lines = String.split_on_char('\n', src);
  let rec find = (row, k, lines) =>
    switch (lines) {
    | [] => None
    | [l, ...rest] =>
      let rec cols = (from, k) =>
        switch (Str.search_forward(Str.regexp_string(needle), l, from)) {
        | c when k == 0 => `Found(c)
        | c => cols(c + 1, k - 1)
        | exception Not_found => `Left(k)
        };
      switch (cols(0, k)) {
      | `Found(c) =>
        Some(
          {
            row,
            col: c + off,
          }: Point.t,
        )
      | `Left(k) => find(row + 1, k, rest)
      };
    };
  find(0, k, lines);
};

/* an action's result, or why it failed */
let perform_why =
    (~root, ~syntax, ~statics, z: Zipper.t, a: Action.t)
    : result(Zipper.t, string) =>
  switch (
    Perform.go(
      ~settings,
      ~statics,
      ~syntax,
      ~root,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => Ok(z)
  | Error(f) => Error(Action.Failure.show(f))
  | exception e => Error("raised " ++ Printexc.to_string(e))
  };

/* [src] loaded, the caret at [needle] (+[off]), then [acts] performed;
   with the loaded program's statics */
let act_st =
    (~root, ~k=0, ~off=1, src: string, needle: string, acts: list(Action.t))
    : (result(Zipper.t, string), CachedStatics.t) => {
  let z0 = load(~root, src);
  let statics = statics_of(~root, z0);
  switch (point_of(~k, ~off, src, needle)) {
  | None => (Error("no " ++ needle), statics)
  | Some(pt) => (
      List.fold_left(
        (z, a) =>
          Stdlib.Result.bind(z, z =>
            perform_why(
              ~root,
              ~syntax=CachedSyntax.init(~root, z),
              ~statics,
              z,
              a,
            )
          ),
        Ok(z0),
        [Action.Move(Point(pt, None)), ...acts],
      ),
      statics,
    )
  };
};

let act = (~root, ~k=0, ~off=1, src, needle, acts): option(Zipper.t) =>
  Stdlib.Result.to_option(fst(act_st(~root, ~k, ~off, src, needle, acts)));

let caret_at = (~root, z: Zipper.t): Point.t =>
  Zipper.Caret.point(CachedSyntax.init(~root, z).measured, z);
