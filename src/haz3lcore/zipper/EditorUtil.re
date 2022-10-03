let editor_of_code = (~read_only=false, init_id, code: CodeString.t) => {
  switch (Printer.zipper_of_string(init_id, code)) {
  | None => None
  | Some((z, new_id)) => Some((new_id, Editor.init(~read_only, z)))
  };
};

let editors_for =
    (~read_only=false, xs: list('a), f: 'a => option(string))
    : (Id.t, int, list(('a, option(Editor.t)))) => {
  let (id_gen, zs) =
    List.fold_left(
      ((acc_id, acc_zs), a) => {
        switch (f(a)) {
        | Some(str) =>
          switch (Printer.zipper_of_string(acc_id, str)) {
          | None => (acc_id, acc_zs @ [(a, Some(Zipper.init(0)))])
          | Some((z, new_id)) => (new_id, acc_zs @ [(a, Some(z))])
          }
        | None => (acc_id, acc_zs @ [(a, None)])
        }
      },
      (0, []),
      xs,
    );
  (
    id_gen,
    0,
    List.map(
      ((a, sz)) =>
        switch (sz) {
        | Some(z) => (a, Some(Editor.init(z, ~read_only)))
        | None => (a, None)
        },
      zs,
    ),
  );
};

let editors_of_strings = (~read_only=false, xs: list(string)) => {
  let (id, i, aes) = editors_for(xs, x => Some(x), ~read_only);
  (id, i, List.map(((_, oe)) => Option.get(oe), aes));
};

let info_map = (editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let info_map = Statics.mk_map(term);
  info_map;
};

let stitch = (editors: list(Editor.t)) => {
  let join_tile = (id): Tile.t => {
    id: id + 10_000_000, // TODO fresh id generation hack
    label: [";"],
    mold: Mold.mk_bin(10, Exp, []),
    shards: [0],
    children: [],
  };
  let segments =
    List.map(
      (ed: Editor.t) => Zipper.unselect_and_zip(ed.state.zipper),
      editors,
    );
  let semicolons =
    List.init(List.length(segments) - 1, i => [Piece.Tile(join_tile(i))]);
  let stitched_segment =
    List.flatten(Util.ListUtil.interleave(segments, semicolons));
  let term = MakeTerm.go(stitched_segment);
  term;
};
