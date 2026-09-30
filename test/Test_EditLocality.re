open Alcotest;
open Haz3lcore;

/* what each incremental layer redoes for one edit: an identity slip costs
   whole-program time, not a wrong answer, so these bounds catch it */

let settings = {
  ...Language.CoreSettings.off,
  statics: true,
};

let statics_of = (z: Zipper.t): CachedStatics.t =>
  CachedStatics.init_from_term(
    ~settings,
    ~is_dynamic_term=true,
    MakeTerm.from_zip_for_sem(z, ~root=Exp).term,
  );

let perform = (~statics, z: Zipper.t, a: Action.t): Zipper.t =>
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

let at = (~statics, z, row, col) =>
  perform(
    ~statics,
    z,
    Move(
      Point(
        {
          row,
          col,
        },
        None,
      ),
    ),
  );

/* top-level pieces of [after]: pointer-equal to [before]'s piece with
   the same id, or new */
let survival = (before: Segment.t, after: Segment.t): (int, int) => {
  let tbl = Hashtbl.create(List.length(before));
  List.iter(p => Hashtbl.replace(tbl, Piece.id(p), p), before);
  List.fold_left(
    ((kept, fresh), p) =>
      switch (Hashtbl.find_opt(tbl, Piece.id(p))) {
      | Some(old) when old === p => (kept + 1, fresh)
      | Some(_) => (kept, fresh)
      | None => (kept, fresh + 1)
      },
    (0, 0),
    after,
  );
};

type work = {
  pieces: int,
  kept: int,
  fresh: int,
  reparsed: int, /* go_incr items */
  fell_back: int, /* go_incr runs that gave up and parsed whole */
  resliced: int, /* term_of items */
  remeasured: int, /* Measured.Incr chunks */
  analyzed: int /* DefStatics items and members */
};

let work = (before: Segment.t, after: Segment.t): work => {
  let (kept, fresh) = survival(before, after);
  let cache = MakeTerm.Incr.mk_cache();
  let fb0 = MakeTerm.Incr.fell_back^;
  let t0 = MakeTerm.Incr.go_incr(~cache, before).term;
  let a0 = MakeTerm.Incr.full_analyzed^;
  let t1 = MakeTerm.Incr.go_incr(~cache, after).term;
  let reparsed = MakeTerm.Incr.full_analyzed^ - a0;
  let fell_back = MakeTerm.Incr.fell_back^ - fb0;
  ignore(MakeTerm.Incr.term_of(before));
  ignore(MakeTerm.Incr.term_of(after));
  let resliced = MakeTerm.Incr.analyzed^;
  let measure = (cache, seg) =>
    ignore(
      Measured.Incr.of_segment(~cache, seg, Id.Map.empty, Id.Map.empty),
    );
  let mc = Measured.Incr.mk_cache();
  measure(mc, before);
  let b0 = Measured.Incr.built^;
  measure(mc, after);
  let remeasured = Measured.Incr.built^ - b0;
  let prev = DefStatics.calc(~settings, t0);
  ignore(DefStatics.calc(~settings, ~prev, t1));
  {
    pieces: List.length(after),
    kept,
    fresh,
    reparsed,
    fell_back,
    resliced,
    remeasured,
    analyzed: DefStatics.last_analyzed^,
  };
};

/* fallbacks: times sparse normalization fell back to the global pass */
type edit = {
  before: Segment.t,
  after: Segment.t,
  fallbacks: int,
};

let edit = (~statics, z: Zipper.t, a: Action.t): edit => {
  let f0 = Zipper.sparse_fallbacks^;
  let z' = perform(~statics, z, a);
  {
    before: Zipper.unselect_and_zip(z),
    after: Zipper.unselect_and_zip(z'),
    fallbacks: Zipper.sparse_fallbacks^ - f0,
  };
};

let corpus = (file: string): Zipper.t =>
  switch (CorpusUtil.corpus_seg(~root=Exp, file)) {
  | Some(seg) => Zipper.unzip(seg)
  | None => fail("corpus unreadable: " ++ file)
  };

/* mega-1k, caret in DewLedger's update at `m.dew < 10|` */
let dew = lazy(corpus("mega-1k.hz"));
let dew_statics = lazy(statics_of(Lazy.force(dew)));
let in_update = (action, col) => {
  let statics = Lazy.force(dew_statics);
  edit(~statics, at(~statics, Lazy.force(dew), 54, col), action);
};

let insert = () => in_update(Insert("0"), 23);
let delete = () => in_update(Destruct(Local(Left, ByChar)), 23);
let paste = () => in_update(Paste("5 + "), 21);
let undo = () => {
  let e = insert();
  {
    ...e,
    before: e.after,
    after: e.before,
  };
};

/* the outline row at [path] (labels from the top) */
let row = (term: Language.Exp.t, path: list(string)): Id.t => {
  let rec go = (nodes: list(Web.OutlineTree.node), path) =>
    switch (path) {
    | [] => None
    | [l, ...rest] =>
      List.find_opt((n: Web.OutlineTree.node) => n.o_label == l, nodes)
      |> Option.map((n: Web.OutlineTree.node) =>
           rest == [] ? n.o_id : go(n.o_children, rest)
         )
      |> Option.join
    };
  switch (go(Web.OutlineTree.of_term(term), path)) {
  | Some(id) => id
  | None => fail("no row " ++ String.concat("/", path))
  };
};

/* deep in the program, so the tiles passed on the way down count */
let restructure = () => {
  let seg = Zipper.unselect_and_zip(Lazy.force(dew));
  let term = MakeTerm.Incr.term_of(seg);
  switch (
    Web.ItemEdit.apply(MoveDown, row(term, ["MetaRunner", "init"]), seg)
  ) {
  | Some((after, _)) => {
      before: seg,
      after,
      fallbacks: 0,
    }
  | None => fail("move failed")
  };
};

let agent = () =>
  edit(
    ~statics=Lazy.force(dew_statics),
    Lazy.force(dew),
    Structural(
      Update(Definition, "final", "let r = MetaRunner.run_all(()) in r"),
    ),
  );

let mega_2k_insert = (row, ()) => {
  let z = corpus("mega-2k.hz");
  let statics = statics_of(z);
  edit(~statics, at(~statics, z, row, 6), Insert(" "));
};

/* bounds: pieces the edit may replace, and items or chunks each layer
   may redo; whole-program work would be tens */
let case = (name, ~fresh=2, ~items=2, ~analyzed=3, ~typing=false, mk) =>
  test_case(
    name,
    `Quick,
    () => {
      let e = mk();
      let w = work(e.before, e.after);
      Printf.printf(
        "LOCALITY %s: pieces=%d kept=%d fresh=%d reparsed=%d resliced=%d remeasured=%d analyzed=%d fallbacks=%d\n",
        name,
        w.pieces,
        w.kept,
        w.fresh,
        w.reparsed,
        w.resliced,
        w.remeasured,
        w.analyzed,
        e.fallbacks,
      );
      check(bool, "pieces kept", true, w.kept >= w.pieces - fresh);
      check(bool, "fresh pieces", true, w.fresh <= fresh);
      check(int, "go_incr fallbacks", 0, w.fell_back);
      check(bool, "go_incr items", true, w.reparsed <= items);
      check(bool, "term_of items", true, w.resliced <= items);
      check(bool, "measured chunks", true, w.remeasured <= items);
      check(bool, "DefStatics items", true, w.analyzed <= analyzed);
      if (typing) {
        check(int, "sparse normalization", 0, e.fallbacks);
      };
    },
  );

let tests = (
  "EditLocality",
  [
    case("insert", ~typing=true, insert),
    case("delete", ~typing=true, delete),
    case("paste", paste),
    case("undo", undo),
    case("restructure", ~analyzed=8, restructure),
    case("agent update", agent),
    case("mega-2k row 900", ~typing=true, mega_2k_insert(900)),
    case("mega-2k row 100", ~typing=true, mega_2k_insert(100)),
  ],
);
