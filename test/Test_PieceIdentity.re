open Alcotest;
open Haz3lcore;

/* Regression gate: top-level piece IDENTITY must survive an insert.
   All the master-view incrementality (Measured.Incr, chunked views,
   go_incr) keys on pointer-equal pieces; before remold_regrout's
   identity restore, one keystroke re-minted every piece (232/232) and
   silently degraded every layer to O(program) per edit.
     bash test/run_node.sh test 'PieceIdentity' */

let corpus_seg = CorpusUtil.corpus_seg(~root=Exp);

let settings = {
  ...Language.CoreSettings.off,
  statics: true,
};

let perform = (z: Zipper.t, a: Action.t): Zipper.t => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let statics =
    CachedStatics.init_from_term(~settings, ~is_dynamic_term=true, term);
  switch (
    Perform.go(
      ~settings,
      ~statics,
      ~syntax=CachedSyntax.init(z),
      ~root=Exp,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => z
  | Error(err) => failwith("action failed: " ++ Action.Failure.show(err))
  };
};

/* identity survival of top-level pieces, aligned by id */
let survival = (before: Segment.t, after: Segment.t): (int, int, int) => {
  let tbl = Hashtbl.create(List.length(before));
  List.iter(p => Hashtbl.replace(tbl, Piece.id(p), p), before);
  let (same, changed, fresh) =
    List.fold_left(
      ((s, c, f), p) =>
        switch (Hashtbl.find_opt(tbl, Piece.id(p))) {
        | Some(old) when old === p => (s + 1, c, f)
        | Some(_) => (s, c + 1, f)
        | None => (s, c, f + 1)
        },
      (0, 0, 0),
      after,
    );
  (same, changed, fresh);
};

let case = (file: string, row: int, ()) =>
  switch (corpus_seg(file)) {
  | None => fail("corpus unreadable: " ++ file)
  | Some(seg) =>
    let z0 = Zipper.unzip(seg);
    let z1 =
      perform(
        z0,
        Move(
          Point(
            {
              row,
              col: 6,
            },
            None,
          ),
        ),
      );
    let before = Zipper.unselect_and_zip(z1);
    let z2 = perform(z1, Insert(" "));
    let after = Zipper.unselect_and_zip(z2);
    let (same, changed, fresh) = survival(before, after);
    Printf.printf(
      "IDENTITY %s@row%d: top-level pieces before=%d after=%d same=%d changed=%d fresh=%d\n",
      file,
      row,
      List.length(before),
      List.length(after),
      same,
      changed,
      fresh,
    );
    /* also: how localized are the incremental layers on this edit? */
    let tc = MakeTerm.Incr.mk_cache();
    let _ = MakeTerm.Incr.go_incr(~cache=tc, before);
    let a0 = MakeTerm.Incr.full_analyzed^;
    let _ = MakeTerm.Incr.go_incr(~cache=tc, after);
    Printf.printf(
      "IDENTITY %s@row%d: go_incr reparsed %d items\n",
      file,
      row,
      MakeTerm.Incr.full_analyzed^ - a0,
    );
    check(
      bool,
      "identity survives (same >= n-2)",
      true,
      same >= List.length(before) - 2,
    );
    check(bool, "fresh pieces bounded", true, fresh <= 2);
    check(
      bool,
      "go_incr localizes (<= 2 items)",
      true,
      MakeTerm.Incr.full_analyzed^ - a0 <= 2,
    );
  };

let tests = (
  "PieceIdentity",
  [
    test_case("mega-2k row 900", `Quick, case("mega-2k.hz", 900)),
    test_case("mega-2k row 100", `Quick, case("mega-2k.hz", 100)),
  ],
);
